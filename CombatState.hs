module CombatState where

import qualified Monster as M
import qualified Player as P
import qualified Data.Map as Map
import Types as T
import Rdice
import Control.Monad.State
import Control.Monad
import Data.Maybe(isJust,fromJust)

type CombatState = State (P.Player, M.Monster)

modifyPlayer :: (P.Player -> P.Player) -> CombatState ()
modifyPlayer f = get >>= \(p,m) -> put $ (f p, m)

modifyMonster :: (M.Monster -> M.Monster) -> CombatState ()
modifyMonster f = get >>= \(p,m) -> put $ (p, f m)

--Apply all modifiers relevant to combat: songs, alertness,
--stealth, etc
applyCombatModifiers :: CombatState ()
applyCombatModifiers = do
  applySongs
  applySlays --applied here because it affects light radius
  updateDarkResistance
  applyLightPenalties
  applyAssassination
  applyHardiness
  applyAlertness
  applyBrands
  applyCritAbilities
  applyCritRes

divBy x y =  y `quot` x

--Net light strength on the player's square
playerNetLight :: P.Player -> M.Monster -> Int
playerNetLight p m =
  P.playerLight p + P.dungeonLight p + M.lightRadius m

--Net light strength on the monster's square
monsterNetLight :: P.Player -> M.Monster -> Int
monsterNetLight p m =
  --glowing monsters are always visible
  if M.glows m 
    then 1 
    else
    P.playerLight p - 1 + M.dungeonLight m + monsterLight 
  where monsterGlow = if M.glows m then 1 else 0
        monsterLight 
          | M.lightRadius m < 0 = M.lightRadius m - 1
          | M.lightRadius m > 0 = M.lightRadius m + 1
          | otherwise = 0

applyCritAbilities :: CombatState ()
applyCritAbilities = do
  (p,_) <- get
  let critMod = P.playerCritMods p 
  modifyPlayer $ P.modifyCritThreshold (+ critMod)
    



applyCritRes :: CombatState ()
applyCritRes = do
  (p,m) <- get
  when (p `P.hasAbility` T.CritRes) $ do
    let penalty = fromIntegral $ max 0 $ (P.will p) `quot` 5
    modifyMonster $ M.modifyCritThreshold (+ penalty)
  --Doubling the critical threshold is equivalent with halving the number
  --of crit bonus dice and rounding downwards.
  when (M.CritResistant == M.criticalRes m) $ do
    modifyPlayer $ P.modifyCritThreshold (* 2)
  when (M.CritImmune == M.criticalRes m) $ do
    let pAttacks = map setNoCrit (P.attacks p)
        setNoCrit a = a {T.canCrit = False} 
    modifyPlayer $ \p -> p {P.attacks = pAttacks}

applyAssassination :: CombatState ()
applyAssassination = do
  (p,m) <- get
  when ((M.seenByPlayer m) && M.Alert /= M.alertness m
          && p `P.hasAbility` T.Assassination) $
    modifyPlayer $ P.modifyAccuracyWith (+ (P.stealth p))


applyBrands :: CombatState ()
applyBrands = do
  (p,m) <- get
  let pRes = P.resistances p
      mRes = M.resistances m
      monsterAttacks = map (applyBrand pRes) (M.attacks m)
      playerAttacks = map (applyBrand mRes) (P.attacks p)
  modifyPlayer $ \p -> p {P.attacks = playerAttacks}
  modifyMonster $ \m -> m {M.attacks = monsterAttacks}

applyBrand :: Map.Map T.Element Int -> T.Attack -> T.Attack
applyBrand rMap attack =
  let brands = T.brands attack
      damDice = T.damage attack
      totalExtraDice = sum $ (map getExtraDice) brands
      getExtraDice element 
        | (rMap Map.! element) == 0 = 1
        | (rMap Map.! element) < 0 = 2
        | otherwise = 0
  in attack { T.damage = addDice totalExtraDice damDice }

--Monsters take evasion penalties for being unwary or asleep.
applyAlertness :: CombatState ()
applyAlertness = do
  (_,m) <- get
  case (M.alertness m) of
    M.Alert -> return ()
    M.Unwary -> modifyMonster $ M.modifyEvasionWith (divBy 2)
    M.Sleeping -> modifyMonster $ \m -> m { M.evasion = -5}

--Extra protection if the player has the ability Hardiness
applyHardiness :: CombatState ()
applyHardiness = do
  (p,_) <- get
  let currentProt = P.protDice p
      extraProtSides =  (P.will p) `quot` 6
      extraProt = 1 `d` extraProtSides
      updatedProt = extraProt : currentProt
  when (p `P.hasAbility` T.Hardiness) $
    modifyPlayer $ \p -> p {P.protDice = updatedProt}


--Apply song effects
applySongs :: CombatState ()
applySongs = do
  (p,_) <- get
  when (SongTrees `P.isActiveSong` p) $ modifyPlayer P.applySongTrees
  when (SongStay `P.isActiveSong` p) $  modifyPlayer P.applySongStay
  when (SongSharp `P.isActiveSong` p) $ modifyPlayer P.applySongSharp

--All light calculations assume that there is only one monster nearby
--and that the player is standing next to that monster.
updateDarkResistance :: CombatState ()
updateDarkResistance = do
  (p,m) <- get
  let darkRes = max 0 $ (playerNetLight p m) - 2
      newMap = (Map.insert Dark darkRes) (P.resistances p)
  put $ (p {P.resistances = newMap}, m)

applyLightPenalties :: CombatState ()
applyLightPenalties = do
  (p,m) <- get
  let monsterVisibilityThreshold = if p `P.hasAbility` T.KeenSenses 
                                    then -1 else 0
  --Can the player see the monster? If the monster's square is too dark, or
  --if the monster has been forced to be invisible with the --minvisible 
  --command line flag, halve player's melee and evasion scores
  when (monsterNetLight p m < monsterVisibilityThreshold
    || not (M.seenByPlayer m)) $ do
    modifyPlayer $ P.modifyEvasionWith (divBy 2)
    modifyPlayer $ P.modifyAccuracyWith (divBy 2)
    modifyMonster $ \m -> m {M.seenByPlayer = False}
  when (M.hatesLight m) $ do
    let lightPenalty = max 0 $ monsterNetLight p m - 2
        applyPenalty = (\s -> s - lightPenalty)
    modifyMonster $ M.modifyEvasionWith applyPenalty
    modifyMonster $ M.modifyAccuracyWith applyPenalty

--If the player is wielding a weapon that slays the monster, add
--a damage die and increase the player's light radius
applySlays :: CombatState ()
applySlays = do
  (p,_) <- get
  adjustedAttacks <- mapM applySlay (P.attacks p)
  modifyPlayer $ \p -> p {P.attacks = adjustedAttacks}

applySlay :: Attack -> CombatState Attack
applySlay attack = do
  (p,m) <- get
  let slainBy = M.slainBy m
      weaponSlays = T.slays attack
      newDamage = addDice 1 (T.damage attack)
      newLightRad = P.lightRadius p + 1
  if (isJust slainBy && (fromJust slainBy) `elem` weaponSlays) 
  then do
    modifyPlayer $ \p -> p {P.lightRadius = newLightRad}
    return $ attack {T.damage = newDamage} 
  else
    return attack
    



