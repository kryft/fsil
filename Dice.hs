module Dice (Dice, 
             d, 
             dist,
             nDice,
             nSides,
             Dist,
             sumDiceDists, 
             fromTuple, 
             mean, 
             std, 
             pprint, 
             addDice) where 

import Control.Monad
import Control.Applicative
import Numeric
import Data.List
import qualified Numeric.Probability.Distribution as D
import qualified Numeric.Probability.Percentage as Percentage


data Dice = MkDice {dist :: D.T Float Int, nDice :: Int, nSides :: Int}
type Dist = D.T Float Int


d :: Int -> Int -> Dice 
d number sides = MkDice diceDist number sides 
  where
  diceDist = sumDiceDists dicelist
  dicelist = take number $ repeat (die sides)

die :: Int -> Dice
die sides = MkDice {dist = D.uniform [1..sides], nDice = 1, nSides = sides}

sumDiceDists :: [Dice] -> Dist
sumDiceDists dice = D.norm $ foldr sumWithNorm (D.certainly 0) diceDists
  where
  diceDists = dist <$> dice
  sumWithNorm d1 d2 = D.norm $ liftM2 (+) d1 d2

addDice :: Int -> Dice -> Dice
addDice nExtraDice origDice = newNDice `d` (nSides origDice)
  where
  newNDice = (nDice origDice) + nExtraDice
 


fromTuple :: (Int, Int) -> Dice
fromTuple (dice, sides) = dice `d` sides

mean :: Dice -> Float
mean d = D.expected $ fmap fromIntegral (dist d)

std :: Dice -> Float
std d = D.stdDev $ fmap fromIntegral (dist d)



pprint :: Dice -> Int -> [String]
pprint d decimals =  map printPair $ (D.sortElem . D.decons . D.norm) (dist d)
  where
  printPair (x,p) = show x ++ " " ++ showFFloat (Just decimals) p ""
