-- | Module for the 'Player' type and related functions. 'Player' represents
-- the (state of the) player in Sil, containing 
module Player where

import Types as T
import Rdice
import qualified Data.Map as Map

-- | The base number by which the player's to-hit roll needs to exceed
--a monster's evasion roll for a critical hit to occur. This is 7.0 in
--Sil (and not likely to change).
playerBaseCritThres = 7.0

-- | The state of a player as far as fsil is concerned, so hit points
-- etc are not included, since we're currently just simulating damage
-- distributions for single attacks.
data Player = Player { name :: String,
                       attacks :: [Attack],
                       -- ^ All the player's melee attacks (only one element
                       -- unless the player has certain abilities).
                       evasion :: Int,
                       -- ^ Evasion score.
                       protDice :: [Dice],
                       -- ^ All the protection dice that the player has cu
                       lightRadius :: Int,
                       activeSongs :: Singing,
                       -- ^ Songs currently sung by the player.
                       resistances :: Map.Map Element Int,
                       abilities :: [Ability],
                       equipment :: [Equipment],
                       -- ^ List of equipped items.
                       will :: Int,
                       song :: Int,
                       stealth :: Int,
                       onLitSquare :: Bool -- ^Standing on a lit square?
                     } deriving (Show)

blankPlayer = Player { name = "",
                       attacks = [],
                       evasion = 0,
                       protDice = [],
                       lightRadius = 0,
                       activeSongs = Quiet,
                       resistances = T.noResistance,
                       abilities = [],
                       equipment = [],
                       will = 0,
                       song = 0,
                       stealth = 0,
                       onLitSquare = False }


-- | Checks whether the player's off-hand is empty (matters for
-- the Subtlety ability).
hasEmptyOffHand :: Player -> Bool
hasEmptyOffHand p = (T.eqName offHandItem) == "(nothing)"
  where 
    offHandItem = head $ filter isOffHand (equipment p)
    isOffHand x = (T.eqSlot x) == T.OffHand

-- | Sets the player's activeSongs to the parameter.
singing :: Singing -> Player -> Player
singing singing' p = p {activeSongs = singing'}

-- | Gives the player additional abilities.
withAbilities :: [T.Ability] -> Player -> Player
withAbilities as p = p {abilities = (abilities p) ++ as}

-- | Checks whether the given song is currently being sung.
isActiveSong :: Song -> Player -> Bool
isActiveSong song' player =
  case (activeSongs player) of 
    Quiet -> False
    OneSong s -> song' == s
    WovenThemes (s1, s2) -> (s1 == song' || s2 == song') 

-- | Does the player have this ability?
hasAbility :: Player -> Ability -> Bool
hasAbility p a = a `elem` (abilities p)

-- | Light strength on player's square (not including effects from
--the environment or monsters)
playerLight :: Player -> Int
playerLight player = (lightRadius player) + 1 + innerLight 
  where
    innerLight = if InnerLight `elem` (abilities player)
                  then 1 else 0

-- Apply effects of various songs; should actually maybe be in
-- CombatState.hs, since these are really only used there.
applySongTrees :: Player -> Player
applySongTrees p =
  let extraLight = (getSongValue SongTrees p) `quot` 5
      light = lightRadius p
  in p {lightRadius = light + extraLight}

applySongStay :: Player -> Player
applySongStay p =
  let extraProtSides = (getSongValue SongStay p) `quot` 3
      newProt = (1 `d` extraProtSides) : (protDice p)
      extraWill = extraProtSides
      newWill = (will p) + extraWill
  in p {protDice = newProt, will = newWill}

applySongSharp :: Player -> Player
applySongSharp p =
  let sharpnessBonus = max 6 $ getSongValue SongSharp p
      extraSharpness = 6.0 / (fromIntegral sharpnessBonus)
      newAttacks = map multiplySharpness (attacks p) :: [Attack]
      multiplySharpness a = 
        a {T.sharpness = (T.sharpness a) * extraSharpness}
  in p {attacks = newAttacks}


dungeonLight :: Player -> Int
dungeonLight p = if (onLitSquare p) then 1 else 0

modifyEvasionWith :: (Int -> Int) -> Player -> Player 
modifyEvasionWith func player = player {evasion = func $ evasion player}

modifyAccuracyWith :: (Int -> Int) -> Player -> Player
modifyAccuracyWith func player =
  let newAttacks = Prelude.map (adjustAccuracy func) (attacks player)
      adjustAccuracy func attack = attack { accuracy = func $ accuracy attack}
  in player {attacks = newAttacks}

modifyCritThreshold :: (Double -> Double) -> Player -> Player
modifyCritThreshold func player = 
  let newAttacks = Prelude.map (adjustCritThres func) (attacks player)
      adjustCritThres f a = a { critThreshold = f $ critThreshold a }
  in player {attacks = newAttacks}


     

-- | Returns the effective song strength for the given Song:
--the player's song score if the player is currently singing that song
--(or song score - 5 if the song is a minor theme) and 0 otherwise.
getSongValue :: Song -> Player -> Int
getSongValue song' player =
  case (activeSongs player) of 
    Quiet -> 0
    OneSong s -> 
      if song' == s then (song player) else 0
    WovenThemes (s1, s2) 
      | s1 == song' -> max 0 $ song player
      | s2 == song' -> max 0 $ (song player) - 5
      | otherwise -> 0

-- | Calculates a player's critical threshold modifiers from abilities
-- (Power, Finesse and Subtlety). The weapon's contribution is computed
-- elsewhere.
playerCritMods :: Player -> Double
playerCritMods player =
  let finesse = if player `hasAbility` T.Finesse
                  then -1.0
                  else 0
      subtlety = if player `hasAbility` T.Subtlety 
                    && hasEmptyOffHand player
                   then -2.0
                   else 0
      power = if player `hasAbility` T.Power
                then 1.0
                else 0
  in finesse + subtlety + power

