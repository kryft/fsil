module Dice (Dice(ZeroDie), 
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

type Dist = D.T Float Int

data Dice = ZeroDie | MkDice Dist Int Int

dist ZeroDie = D.certainly 0
dist (MkDice dist' _ _) = dist'

nDice ZeroDie = 0
nDice (MkDice _ nDice' _) = nDice'

nSides ZeroDie = 0
nSides (MkDice _ _ nSides') = nSides'

instance Show Dice where
  show d = show (nDice d) ++ "d" ++ show (nSides d)

instance Eq Dice where
  ZeroDie == ZeroDie = True
  (MkDice _ nDiceX nSidesX) == (MkDice _ nDiceY nSidesY)
    = (nDiceX == nDiceY) && (nSidesX == nSidesY)
  _ == _ = False
  



d :: Int -> Int -> Dice 
d number sides 
  | number > 0 && sides > 0 = MkDice diceDist number sides 
  | otherwise = ZeroDie
  where
  diceDist = sumDists distlist
  distlist = take number $ repeat $ D.uniform [1..sides]

sumDists :: [Dist] -> Dist
sumDists dists = D.norm $ foldr sumWithNorm (D.certainly 0) dists
  where
  sumWithNorm d1 d2 = D.norm $ liftM2 (+) d1 d2



sumDiceDists :: [Dice] -> Dist
sumDiceDists dice = sumDists diceDists
  where
  diceDists = dist <$> dice
  sumWithNorm d1 d2 = D.norm $ liftM2 (+) d1 d2

addDice :: Int -> Dice -> Dice
addDice nExtraDice origDice 
  | nExtraDice > 0 = newNDice `d` (nSides origDice)
  | otherwise = origDice
  where
  newNDice = (nDice origDice) + nExtraDice
 


fromTuple :: (Int, Int) -> Dice
fromTuple (dice, sides) = dice `d` sides

mean :: Dist -> Float
mean d = D.expected $ fmap fromIntegral d

std :: Dist -> Float
std d = D.stdDev $ fmap fromIntegral d



pprint :: Dice -> Int -> [String]
pprint d decimals =  map printPair $ (D.sortElem . D.decons . D.norm) (dist d)
  where
  printPair (x,p) = show x ++ " " ++ showFFloat (Just decimals) p ""
