import Control.Monad
import Numeric
import Data.List ((!!))
import qualified Numeric.Probability.Distribution as D
import qualified Numeric.Probability.Percentage as Percentage
import qualified Monster as M
import MonsterParser

import Dice

monsBaseCritThres = 7

data FightStats = FightStats { damGiven :: Dist, 
                               damTaken :: Dist, 
                               player :: String, 
                               opponent :: M.Monster
                             }



toHitDist :: Int -> Int -> Dist
toHitDist accuracy evasion = 
  do 
    toHitRoll <- dist $ 1 `d` 20
    evasionRoll <- dist $ 1 `d` 20
    return $ (toHitRoll + accuracy) - (evasionRoll + evasion)


nCrits :: Double -> Int -> Int
nCrits baseCriticalThreshold toHitRoll 
  | toHitRoll < 1 = 0
  | otherwise =
    round $ (fromIntegral toHitRoll) / baseCriticalThreshold

damageDist :: Dice -> [Dice] -> Dist
damageDist damDice protDiceList =
  do
    damage <- dist damDice
    protection <- sumDiceDists protDiceList
    return $ max 0 (damage - protection)

attackDist :: Int -> Dice -> Double -> Int -> [Dice] -> Dist
attackDist accuracy damDice baseCriticalThreshold evasion 
  protectionDice =
  do
    toHit <- D.norm $ toHitDist accuracy evasion
    if toHit < 1 then
      D.certainly 0
    else do
      let damDice' = addDice nCritDice damDice
          nCritDice = nCrits baseCriticalThreshold toHit
      D.norm $ damageDist damDice' protectionDice


fight :: Int -> Dice -> Int -> [Dice] -> Double -> String -> Int -> [M.Monster] -> FightStats
fight acc damDice ev protDice baseCritThres monsName monsAttackNumber monsList =
  let opponent' = M.getMonster monsName monsList
      (oppAcc,oppDam) = (M.monsAttacks opponent') !! monsAttackNumber
      oppEv = M.monsEvasion opponent'
      oppProt = M.monsProtDice opponent'
      damGiven' = attackDist acc damDice baseCritThres oppEv [oppProt]
      oppBaseCritThres = monsBaseCritThres + 2 * fromIntegral (nDice oppDam)
      damTaken' = attackDist oppAcc oppDam oppBaseCritThres ev protDice
  in
    FightStats { damGiven = damGiven', damTaken = damTaken', 
                 player = "player", opponent = opponent' }
     
