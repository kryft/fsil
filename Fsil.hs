import Control.Monad
import Numeric
import Data.List
import qualified Numeric.Probability.Distribution as D
import qualified Numeric.Probability.Percentage as Percentage

import Dice

toHitRoll :: Int -> Int -> Dist
toHitRoll accuracy evasion = 
  do 
    toHitRoll' <- dist $ 1 `d` 20
    evasionRoll <- dist $ 1 `d` 20
    return $ (toHitRoll' + accuracy) - (evasionRoll + evasion)


nCrits :: Double -> Int -> Int
nCrits baseCriticalThreshold toHitRoll 
  | toHitRoll < 1 = 0
  | otherwise =
    round $ (fromIntegral toHitRoll) / baseCriticalThreshold

damageRoll :: Dice -> [Dice] -> Dist
damageRoll damDice protDiceList =
  do
    damage <- dist damDice
    protection <- sumDiceDists protDiceList
    return $ max 0 (damage - protection)

attackRoll :: Int -> Dice -> Double -> Int -> [Dice] -> Dist
attackRoll accuracy damDice baseCriticalThreshold evasion 
  protectionDice =
  do
    toHit <- D.norm $ toHitRoll accuracy evasion
    if toHit < 1 then
      return 0
    else do
      let damDice' = addDice nCrits' damDice
          nCrits' = nCrits baseCriticalThreshold toHit
      D.norm $ damageRoll damDice' protectionDice


