module Main (fight,
             FightStats,
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


data FightStats = FightStats { damGiven :: Dist, 
                               critsGiven :: [Dist],
                               damGivenPercent :: Dist, 
                               damTaken :: [Dist], 
                               player :: P.Player, 
                               opponent :: M.Monster
                             }


toHitDist :: Int -> Int -> RInt
toHitDist accuracy evasion = 
  do 
    toHitRoll <- dist $ 1 `d` 20
    evasionRoll <- dist $ 1 `d` 20
    return $ (toHitRoll + accuracy) - (evasionRoll + evasion)


nCrits :: Double -> Int -> Int
nCrits criticalThreshold toHitRoll 
  | toHitRoll < 1 = 0
  | otherwise =
    floor $ (fromIntegral toHitRoll + 0.4) / criticalThreshold

damageDist :: Dice -> Double -> [Dice] -> RInt
damageDist damDice sharpness protDiceList =
  do
    damage <-  dist damDice
    protection <-  sumDiceDists protDiceList
    let modifiedProt = floor $ fromIntegral protection * sharpness
    return $ max 0 (damage - modifiedProt)

attackDamDist :: T.Attack -> Int -> [Dice] -> RInt
attackDamDist attack evasion protDice =
  do
   toHit <- if (T.alwaysHits attack)
     then return 1
     else toHitDist (T.accuracy attack) evasion
   if toHit < 1 then
     return 0
   else do
     let damDice = addDice nCritDice (T.damage attack)
         sharpness = T.sharpness attack
         nCritDice = if (T.canCrit attack)
                       then nCrits (T.critThreshold attack) toHit
                       else 0
     damageDist damDice sharpness protDice

attackSeqDamDist :: [T.Attack] -> Int -> [Dice] -> RInt
attackSeqDamDist [] _ _ = return 0
attackSeqDamDist (a:as) evasion protDice = 
  do
    damRest <- attackSeqDamDist as evasion protDice
    dam <- attackDamDist a evasion protDice
    return $ dam + damRest

attackNCritsDist :: T.Attack -> Int -> RInt
attackNCritsDist attack evasion =
  do
   toHit <- toHitDist (T.accuracy attack) evasion
   let nCritDice = if (T.canCrit attack)
                   then nCrits (T.critThreshold attack) toHit
                   else 0
   return nCritDice

attackSeqNCritsDist :: [T.Attack] -> Int -> [RInt]
attackSeqNCritsDist [] _ = []
attackSeqNCritsDist (a:as) evasion =
  (attackNCritsDist a evasion) : attackSeqNCritsDist as evasion

damDistPercent :: RInt -> M.Monster -> RInt
damDistPercent damDist m =
  do
    maxHP <- dist $ (M.health m)
    damage <- damDist
    return $ 100 - (100 * (maxHP - damage)) `quot` maxHP


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
      --When a monster has several attacks, they're mutually exclusive
      --alternatives, so compute separate distributions for each
      damTaken' = map distForOneAttack mAttacks
      distForOneAttack a = attackDamDist a pEv pProt
  in 
    do
      damGiven <- simulate nSamples $! damGiven'
      damGivenPercent <- simulate nSamples $! damGivenPercent'
      critsGiven <- mapM (simulate nSamples) $! critsGiven'
      damTaken <- mapM (simulate nSamples) $! damTaken'
      return $ FightStats { damGiven = damGiven, 
                   damGivenPercent = damGivenPercent,
                   critsGiven = critsGiven,
                 damTaken = damTaken, 
                 player = p, 
                 opponent = m}

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
   ++ "\nPlayer sees monster: " ++ show ( M.seenByPlayer $ opponent fs )
   ++ "\n\n\nMONSTER ATTACKING PLAYER\n"
   ++ "\nProbability of dealing at least x damage: \n\n" 
   ++ unlines (map (printCDF 3) $ map (ccdf 1) (damTaken fs))
   ++ "\n\nPLAYER ATTACKING MONSTER\n"
   ++ "\nProbability of getting at least n critical hits:\n" 
   ++ unlines ( map (printCDF 3) $ map (ccdf 1) (critsGiven fs) )
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
