import Control.Monad
import Numeric
import Data.List
import qualified Numeric.Probability.Distribution as D
import qualified Numeric.Probability.Percentage as Percentage

type Dist = D.T Float Int
d :: Int -> Int -> Dist 
d number sides = sumDice dicelist
  where
  dicelist = take number $ repeat (die sides)


sumDice :: [Dist] -> Dist
sumDice = foldr sumWithNorm (D.certainly 0)
  where
  sumWithNorm d1 d2 = D.norm $ liftM2 (+) d1 d2

fromTuple :: (Int, Int) -> Dist
fromTuple (dice, sides) = dice `d` sides

die :: Int -> Dist 
die sides = D.uniform [1..sides]

toHitRoll :: Int -> Int -> Dist
toHitRoll accuracy evasion = 
  do 
    toHitRoll' <- 1 `d` 20
    evasionRoll <- 1 `d` 20
    return $ (toHitRoll' + accuracy) - (evasionRoll + evasion)


nCrits :: Double -> Int -> Int
nCrits baseCriticalThreshold toHitRoll 
  | toHitRoll < 1 = 0
  | otherwise =
    round $ (fromIntegral toHitRoll) / baseCriticalThreshold

damageRoll :: (Int,Int) -> [(Int,Int)] -> Dist
damageRoll damDice protDiceList =
  do
    damage <- fromTuple damDice
    protection <- sumDice $ map fromTuple protDiceList
    return $ max 0 (damage - protection)

attackRoll :: Int -> (Int,Int) -> Double -> Int -> [(Int,Int)] -> Dist
attackRoll accuracy (damDice,damSides) baseCriticalThreshold evasion 
  protectionDice =
  do
    toHit <- D.norm $ toHitRoll accuracy evasion
    if toHit < 1 then
      return 0
    else do
      let damDice' = damDice + nCrits baseCriticalThreshold toHit
      D.norm $ damageRoll (damDice',damSides) protectionDice

mean :: Dist -> Float
mean d = D.expected $ fmap fromIntegral d

std :: Dist -> Float
std d = D.stdDev $ fmap fromIntegral d

pprint :: Dist -> Int -> [String]
pprint d decimals =  map printPair $ (D.sortElem . D.decons . D.norm) d
  where
  printPair (x,p) = show x ++ " " ++ showFFloat (Just decimals) p ""
