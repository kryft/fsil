module Main where
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
import Dice
import qualified CombatState as CS
import MonsterParser 
import CharDumpParser
import CommandLineArgs as CLA


data FightStats = FightStats { damGiven :: Dist, 
                               critsGiven :: [Dist],
                               damGivenPercent :: Dist, 
                               damTaken :: [Dist], 
                               player :: P.Player, 
                               opponent :: M.Monster
                             }


toHitDist :: Int -> Int -> Dist
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

damageDist :: Dice -> Double -> [Dice] -> Dist
damageDist damDice sharpness protDiceList =
  do
    damage <- dist damDice
    protection <- sumDiceDists protDiceList
    let modifiedProt = floor $ fromIntegral protection * sharpness
    return $ max 0 (damage - modifiedProt)

attackDamDist :: T.Attack -> Int -> [Dice] -> Dist
attackDamDist attack evasion protDice =
  do
   toHit <- if (T.alwaysHits attack)
     then return 1
     else D.norm $ toHitDist (T.accuracy attack) evasion
   if toHit < 1 then
     D.certainly 0
   else do
     let damDice = addDice nCritDice (T.damage attack)
         sharpness = T.sharpness attack
         nCritDice = if (T.canCrit attack)
                       then nCrits (T.critThreshold attack) toHit
                       else 0
     D.norm $ damageDist damDice sharpness protDice

attackSeqDamDist :: [T.Attack] -> Int -> [Dice] -> Dist
attackSeqDamDist [] _ _ = return 0
attackSeqDamDist (a:as) evasion protDice = 
  do
    damRest <- attackSeqDamDist as evasion protDice
    dam <- attackDamDist a evasion protDice
    D.norm $ return $ dam + damRest

attackNCritsDist :: T.Attack -> Int -> Dist
attackNCritsDist attack evasion =
  do
   toHit <- D.norm $ toHitDist (T.accuracy attack) evasion
   let nCritDice = if (T.canCrit attack)
                   then nCrits (T.critThreshold attack) toHit
                   else 0
   return nCritDice

attackSeqNCritsDist :: [T.Attack] -> Int -> [Dist]
attackSeqNCritsDist [] _ = []
attackSeqNCritsDist (a:as) evasion =
  (attackNCritsDist a evasion) : attackSeqNCritsDist as evasion

damDistPercent :: Dist -> M.Monster -> Dist
damDistPercent damDist m =
  do
    maxHP <- dist (M.health m)
    damage <- damDist
    D.norm $ return $ 100 - (100 * (maxHP - damage)) `quot` maxHP


fight :: P.Player -> M.Monster -> FightStats
fight player monster =
  let (_,(p,m)) = runState CS.applyCombatModifiers (player,monster)
      pAttacks = P.attacks p
      pEv = P.evasion p
      pProt = P.protDice p
      mAttacks = M.attacks m
      mEv = M.evasion m
      mProt = M.protDice m
      damGiven = attackSeqDamDist pAttacks mEv [mProt]
      damGivenPercent = damDistPercent damGiven monster
      critsGiven = attackSeqNCritsDist pAttacks mEv
      --When a monster has several attacks, they're mutually exclusive
      --alternatives, so compute separate distributions for each
      damTaken = map distForOneAttack mAttacks
      distForOneAttack a = attackDamDist a pEv pProt
  in
      FightStats { damGiven = damGiven, 
                   damGivenPercent = damGivenPercent,
                   critsGiven = critsGiven,
                 damTaken = damTaken, 
                 player = p, 
                 opponent = m}

summarize :: FightStats -> String
summarize fs = P.name (player fs) ++ " vs " ++ M.name (opponent fs) 
   ++ "\nPlayer dark resistance: " 
   ++ show ( (P.resistances $ player fs) Map.! T.Dark)
   ++ "\nPlayer singing: "
   ++ show (P.activeSongs $ player fs)
   ++ "\nMonster alertness: "
   ++ show (M.alertness $ opponent fs)
   ++ "\nPlayer sees monster: " ++ show ( M.seenByPlayer $ opponent fs )
   ++ "\nDamage dealt by monster: mean " 
   ++ show ( map mean (damTaken fs))
   ++ ", standard deviation " 
   ++ show (  map std (damTaken fs))
   ++ "\nProbability of dealing at least x damage: \n" 
   ++ unlines (map (printCDF 3) $ map (ccdf 1) (damTaken fs))
   ++ "\n\nDamage dealt by player: mean " 
   ++ show (mean (damGiven fs))
   ++ ", standard deviation " ++ show (std (damGiven fs))
   ++ "\nProbability of getting at least n critical hits: \n" 
   ++ unlines ( map (printCDF 3) $ map (ccdf 1) (critsGiven fs) )
   ++ "\nProbability of dealing at least X% (of max hp) damage: \n" 
   ++ (printCDF 3 $ takeWhile ((<= 100) . fst)  $ ccdf 10 $ damGivenPercent fs ) 
   ++ "\nProbability of dealing at least x damage: \n" 
   ++ (printCDF 3 $ ccdf 1 (damGiven fs) )
 where

main = do
  fsilOptions <- CLA.parseArgs
  player <- readCharDump (CLA.charDumpFile fsilOptions)
  let player' = P.singing (CLA.singing fsilOptions) player
      monsterName = CLA.monsterName fsilOptions
      alertness = CLA.alertness fsilOptions
  monsters <- parseMonsterFile "monster.txt"
  let monster = (M.getMonster monsterName monsters) {M.alertness = alertness}
  putStr . summarize $ fight player' monster
