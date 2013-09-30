module Rdice where
import qualified Numeric.Probability.Random as R
import qualified Numeric.Probability.Distribution as D
import qualified Numeric.Probability.Simulation as S
import Control.Monad
import Numeric

type Dist = D.T Float Int

type RInt = R.T Int

data Dice = ZeroDie | MkDice (RInt) Int Int

d :: Int -> Int -> Dice
d number sides 
  | number > 0 && sides > 0 = MkDice diceDist number sides 
  | otherwise = ZeroDie
  where
  diceDist = sumDists distlist
  distlist = replicate number $! R.pick $! (D.uniform [1..sides] :: Dist)

instance Show Dice where
  show d = "fromTuple (" ++ show (nDice d) ++ "," ++ show (nSides d) ++ ")"

sumDists :: [RInt] -> RInt
sumDists dists = foldr (liftM2 (+)) (return 0) dists

sumDiceDists :: [Dice] -> RInt
sumDiceDists dice = sumDists (fmap dist dice)

addDice :: Int -> Dice -> Dice
addDice nExtraDice origDice 
  | nExtraDice > 0 = newNDice `d` (nSides origDice)
  | otherwise = origDice
  where
  newNDice = (nDice origDice) + nExtraDice
 
--simulate :: Int -> RInt -> IO Dist
--simulate n = R.run . R.dist . replicate n

simulate :: Int -> RInt -> IO Dist
simulate n rand_var = 
  (D.norm . D.uniform) `fmap` (simulate' n rand_var)
  where
  simulate' 0 _ = return []
  simulate' n rand_var = do
    sample <- R.run rand_var
    rest <- sample `seq` simulate' (n-1) rand_var
    return $ (sample : rest)
  

  


dist ZeroDie = return 0
dist (MkDice dist' _ _) = dist'

nDice ZeroDie = 0
nDice (MkDice _ nDice' _) = nDice'

nSides ZeroDie = 0
nSides (MkDice _ _ nSides') = nSides'

instance Eq Dice where
  ZeroDie == ZeroDie = True
  (MkDice _ nDiceX nSidesX) == (MkDice _ nDiceY nSidesY)
    = (nDiceX == nDiceY) && (nSidesX == nSidesY)
  _ == _ = False
  

fromTuple :: (Int, Int) -> Dice
fromTuple (dice, sides) = dice `d` sides

mean :: Dist -> Float
mean d = D.expected $ fmap fromIntegral d

std :: Dist -> Float
std d = D.stdDev $ fmap fromIntegral d

minD :: Dist -> Int
minD = minimum . (map fst) . D.decons

maxD :: Dist -> Int
maxD = maximum . (map fst) . D.decons

cdf :: Int -> Dist -> [(Int, Float)]
cdf interval d =
  let minVal = minD d
      maxVal = maxD d
      values = [minVal,minVal+interval .. maxVal]
      probabilities = map (\v -> (< v) D.?? d) values
  in zip values probabilities


ccdf :: Int -> Dist -> [(Int, Float)]
ccdf i d = map (\(x,p) -> (x,1-p)) $ cdf i d

pprint :: Int -> Dist -> String
pprint decimals d = unlines $ map printPair $ (D.sortElem . D.decons . D.norm) d
  where
  printPair (x,p) = show x ++ " " ++ showFFloat (Just decimals) p ""

printCDF :: Int -> [(Int, Float)] -> String 
printCDF decimals c = unlines $ map printPair $ filter aboveProbThres c
  where
  aboveProbThres = if decimals > 0 
                    then (>= 10 ** (- (fromIntegral decimals))) . snd
                    else (> 0) . snd
  printPair (x,p) = if decimals > 0
                      then show x ++ " " ++ showFFloat (Just decimals) p ""
                      else show x ++ " " ++ show p
