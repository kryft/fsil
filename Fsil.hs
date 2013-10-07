-- | A simple fight simulator for Sil 1.1.1; see the README for
-- a more thorough introduction.
module Main (fight,
             FightStats,
             summarize,
             damGiven,
             critsGiven,
             damGivenPercent,
             damTaken,
             player,
             opponent,
             readCharDump,
             parseMonsterFile,
             getMonster,
             main)
             where
import System.Environment
import Control.Monad
import Control.Monad.State
import Numeric
import Data.List (concat)
import qualified Numeric.Probability.Distribution as D
import Numeric.Probability.Distribution((??))
import qualified Monster as M
import Monster(getMonster)
import qualified Player as P
import qualified Data.Map as Map
import qualified Types as T
import Rdice
import qualified CombatState as CS
import MonsterParser 
import CharDumpParser
import CommandLineArgs as CLA
import qualified Numeric.Probability.Random as R
import qualified Numeric.Probability.Transition as TR
import qualified Numeric.Probability.Simulation as S


-- | 'FightStats' contains all the statistics that fsil computes.
data FightStats = FightStats { 
  damGiven :: Dist, 
  -- ^ Distribution of the damage inflicted by the player on the monster
  -- in a single turn (using all the attacks that the player has if 
  -- they have several due to Two-Weapon Fighting or Rapid Attack.)
  critsGiven :: [Dist],
  -- ^ Distributions of the number of extra damage dice that the player
  -- gets due to a critical hit, separately for each of the player's
  -- attacks.
  confusionTurnsInflicted :: Dist,
  -- ^ Distribution of the number of turns of confusion inflicted on
  -- the monster due to a critical hit if the player has the
  -- Cruel Blow ability.
  damGivenPercent :: Dist, 
  -- ^ Distribution of the damage inflicted by the player on the monster
  -- as a percentage of the monster's maximum hit points; monster max hp
  -- is a random number, so this cannot be computed directly from 
  -- @damGiven@ above.
   damTaken :: [Dist], 
   -- ^ Distributions of the damage inflicted on the player by the monster,
   -- separately for each of the monster's attacks if the monster has
   -- several. (Monsters that have several attacks don't attack with both
   -- attacks at the same time.)
  player :: P.Player, 
  opponent :: M.Monster
}


--All the functions that follow use RInt as a monad, as defined in
--Numeric.Probability.Random; 
--http://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf is a good
--explanation of the idea and implementation. 


--Returns the net to-hit roll, i.e. the difference between the attacker's 
--to-hit roll and the defender's evasion roll
netToHitDist :: Int -> Int -> RInt
netToHitDist accuracy evasion = 
  do 
    toHitRoll <- (+accuracy) `fmap` (dist $ 1 `d` 20)
    evasionRoll <- (+evasion) `fmap` (dist $ 1 `d` 20)
    return $ toHitRoll - evasionRoll


--Calculate the number of critical hits based on the base threshold
--and the net to-hit score.
nCrits :: Double -> Int -> Int
nCrits criticalThreshold toHitRoll 
  | toHitRoll < 1 = 0
  | otherwise =
    floor $ (fromIntegral toHitRoll + 0.4) / criticalThreshold


--Roll damage and subtract protection roll (modified by sharpness).
netDamageDist :: Dice -> Double -> [Dice] -> RInt
netDamageDist damDice sharpness protDiceList =
  do
    damage <-  dist damDice
    protection <-  sumDice protDiceList
    let modifiedProt = floor $ fromIntegral protection * sharpness
    return $ max 0 (damage - modifiedProt)

--Take an Attack and the defender's evasion score and return the
--net damage distribution for that attack.
attackDamDist :: T.Attack -> Int -> [Dice] -> RInt
attackDamDist attack evasion protDice =
  do
   toHit <- if (T.alwaysHits attack)
     then return 1
     else netToHitDist (T.accuracy attack) evasion
   if toHit < 1 then
     return 0
   else do
     let damDice = addDice nCritDice (T.damage attack)
         sharpness = T.sharpness attack
         nCritDice = if (T.canCrit attack)
                       then nCrits (T.critThreshold attack) toHit
                       else 0
     netDamageDist damDice sharpness protDice

--Take a list of attacks and the defender's evasion score and protection
--dice, and return the net damage distribution for the attacks being applied
--one after another (e.g. the player attacks the monster with two attacks
--from Rapid Attack or Two-Weapon Fighting)
attackSeqDamDist :: [T.Attack] -> Int -> [Dice] -> RInt
attackSeqDamDist [] _ _ = return 0
attackSeqDamDist (a:as) evasion protDice = 
  do
    damRest <- attackSeqDamDist as evasion protDice
    dam <- attackDamDist a evasion protDice
    return $ dam + damRest

--Take an attack and the defender's evasion score and return the distribution
--of the number of critical dice that the attacker gets.
attackNCritsDist :: T.Attack -> Int -> RInt
attackNCritsDist attack evasion =
  do
   toHit <- netToHitDist (T.accuracy attack) evasion
   let nCritDice = if (T.canCrit attack)
                   then nCrits (T.critThreshold attack) toHit
                   else 0
   return nCritDice

--Take a list of attacks and the defender's evasion score and return the 
--distributions of the number of critical dice that the attacker gets,
--separately for each attack in the list.
attackSeqNCritsDist :: [T.Attack] -> Int -> [RInt]
attackSeqNCritsDist [] _ = []
attackSeqNCritsDist (a:as) evasion =
  (attackNCritsDist a evasion) : attackSeqNCritsDist as evasion

--Number of turns of confusion inflicted with cruel blow.
--If multiple player attacks inflict confusion, the maximum of
--the confusion turn counts inflicted by the individual attacks
--will be used.
cruelBlowTurns :: [RInt] -> M.Monster -> RInt
cruelBlowTurns nCritsDists monster = 
  fmap maximum $ sequence $ map (cBTurnsForOneAttack monster) nCritsDists
  where
  cBTurnsForOneAttack monster nCritsDist = do
    nCrits <- nCritsDist
    check <- skillCheck (4 * nCrits) (M.will monster)
    if check > 0
      then return nCrits
      else return 0


--Roll a skill check.
skillCheck :: Int -> Int -> RInt
skillCheck skill difficulty = do
  skillRoll <- (+ skill) `fmap` (dist $ 1 `d` 10)
  diffRoll <- (+ difficulty) `fmap` (dist $ 1 `d` 10)
  return $ skillRoll - diffRoll

--Take a net damage distribution and return a distribution
--of the damage as a percentage of the monster's max hp.
damDistPercent :: RInt -> M.Monster -> RInt
damDistPercent damDist m =
  do
    maxHP <- dist $ (M.health m)
    damage <- damDist
    return $ 100 - (100 * (maxHP - damage)) `quot` maxHP


--Take a player, a monster and a number of samples to simulate,
--and produce a FightStats. The distributions in FightStats are
--empirical distributions simulated using nSamples samples.
fight :: P.Player -> M.Monster -> Int -> IO FightStats
fight player monster nSamples =
  let (_,(p,m)) = runState CS.applyCombatModifiers (player,monster)
      pAttacks = P.attacks p
      pEv = P.evasion p
      pProt = P.protDice p
      mAttacks = M.attacks m
      mEv = M.evasion m
      mProt = M.protDice m
      damGiven' = attackSeqDamDist pAttacks mEv [mProt]
      damGivenPercent' = damDistPercent damGiven' monster
      critsGiven' = attackSeqNCritsDist pAttacks mEv
      confusionTurnsInflicted' = cruelBlowTurns critsGiven' monster
      --When a monster has several attacks, they're mutually exclusive
      --alternatives, so compute separate distributions for each
      damTaken' = map distForOneAttack mAttacks
      distForOneAttack a = attackDamDist a pEv pProt
  in 
    do
      damGiven <- simulate nSamples $! damGiven'
      damGivenPercent <- simulate nSamples $! damGivenPercent'
      critsGiven <- mapM (simulate nSamples) $! critsGiven'
      confusionTurnsInflicted <- simulate nSamples $! confusionTurnsInflicted'
      damTaken <- mapM (simulate nSamples) $! damTaken'
      return $ FightStats { damGiven = damGiven, 
                 damGivenPercent = damGivenPercent,
                 critsGiven = critsGiven,
                 confusionTurnsInflicted = confusionTurnsInflicted,
                 damTaken = damTaken, 
                 player = p, 
                 opponent = m}

-- | Pretty-print a FightStats
summarize :: FightStats -> String
summarize fs = "SUMMARY AND MISC INFORMATION\n\n" 
   ++ P.name (player fs) ++ " vs " ++ M.name (opponent fs) 
   ++ "\nDamage dealt by monster: mean " 
   ++ show ( map mean (damTaken fs))
   ++ ", standard deviation " 
   ++ show (  map std (damTaken fs))
   ++ "\nDamage dealt by player: mean " 
   ++ show (mean (damGiven fs))
   ++ ", standard deviation " ++ show (std (damGiven fs))
   ++ "\nPlayer dark resistance: " 
   ++ show ( (P.resistances $ player fs) Map.! T.Dark)
   ++ "\nPlayer singing: "
   ++ show (P.activeSongs $ player fs)
   ++ "\nMonster alertness: "
   ++ show (M.alertness $ opponent fs)
   ++ "\nMonster will: "
   ++ show (M.will $ opponent fs)
   ++ "\nPlayer sees monster: " ++ show ( M.seenByPlayer $ opponent fs )
   ++ "\n\n\nMONSTER ATTACKING PLAYER\n"
   ++ "\nProbability of dealing at least x damage: \n\n" 
   ++ unlines (map (printCDF 0) $ map (ccdf 1) (damTaken fs))
   ++ "\n\nPLAYER ATTACKING MONSTER\n"
   ++ "\nProbability of getting at least n critical hits:\n" 
   ++ unlines ( map (printCDF 3) $ map (ccdf 1) (critsGiven fs) )
   ++ "\nProbability of inflicting at least n turns of confusion \
    \with Cruel Blow:\n" 
   ++ (printCDF 3 $ ccdf 1 (confusionTurnsInflicted fs))
   ++ "\nProbability of dealing at least X% (of max hp) damage:\n" 
   ++ (printCDF 3 $ takeWhile ((<= 100) . fst)  $ ccdf 10 $ damGivenPercent fs ) 
   ++ "\nProbability of dealing at least x damage: \n" 
   ++ (printCDF 3 $ ccdf 1 (damGiven fs) )
 where

main = do
  fsilOptions <- CLA.parseArgs
  player <- readCharDump (CLA.charDumpFile fsilOptions)
  let singing' = CLA.singing fsilOptions
      meleeBonus = CLA.meleeBonus fsilOptions
      evBonus = CLA.evBonus fsilOptions
      nSamples = CLA.nSamples fsilOptions
      player' = (P.modifyAccuracyWith (+ meleeBonus))
        . (P.modifyEvasionWith (+ evBonus))
        . (P.singing singing') $ 
          player {P.onLitSquare = CLA.playerOnLitSquare fsilOptions}
      monsterName = CLA.monsterName fsilOptions
      alertness = CLA.alertness fsilOptions
  monsters <- parseMonsterFile "monster.txt"
  let monster = (M.getMonster monsterName monsters) {M.alertness = alertness, 
     M.onLitSquare = CLA.monsterOnLitSquare fsilOptions,
     M.seenByPlayer = not $ CLA.invisibleMonster fsilOptions}
  fightStats <- fight player' monster nSamples
  putStr . summarize $ fightStats
