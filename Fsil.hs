import Control.Monad
import Numeric
import Data.List
import qualified Numeric.Probability.Distribution as D
import qualified Numeric.Probability.Percentage as Percentage

import Dice

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
      return 0
    else do
      let damDice' = addDice nCritDice damDice
          nCritDice = nCrits baseCriticalThreshold toHit
      D.norm $ damageDist damDice' protectionDice


