-- | Dice for Sil implemented using types and functions
-- from 'Numeric.Probability' from probability-0.2.4 on hackage.
module Rdice(Dice(ZeroDie),
             Dist(),
             RInt(),
             nSides,
             nDice,
             dist,
             d,
             fromTuple,
             sumDice,
             addDice,
             simulate,
             mean,
             max,
             min,
             std,
             cdf,
             ccdf,
             printCDF,
             pprint) where
import qualified Numeric.Probability.Random as R
import qualified Numeric.Probability.Distribution as D
import qualified Numeric.Probability.Simulation as S
import Control.Monad
import Numeric

-- | 'Dist' represents a probability distribution - basically a (finite) set 
-- of 'Int' values associated with 'Float' probabilities. 
type Dist = D.T Float Int

-- | An attempt at pretty-printing a 'Dist'
pprint :: Int -> Dist -> String
pprint decimals d = unlines $ map printPair $ (D.sortElem . D.decons . D.norm) d
  where
  printPair (x,p) = show x ++ " " ++ showFFloat (Just decimals) p ""


-- | 'RInt' represents a random integer variable following
-- some distribution.
type RInt = R.T Int

-- | Draw @n@ samples from the distribution of @rand_var@. The result is an
-- empirical distribution in the form of an 'IO Dist'.
simulate :: Int -> RInt -> IO Dist
simulate n rand_var = 
  (D.norm . D.uniform) `fmap` (simulate' n rand_var)
  where
  simulate' 0 _ = return []
  simulate' n rand_var = do
    sample <- R.run rand_var
    rest <- sample `seq` simulate' (n-1) rand_var
    return $ (sample : rest)
  


-- | The 'Dice' type represents a set of M dice with N sides;
-- e.g. 2d6 in traditional RPG notation when M = 2 and N = 6.
-- 'ZeroDie' is a die that always rolls 0 (M = 0 or N = 0).
data Dice = ZeroDie | MkDice (RInt) Int Int

-- | Create @m@ @n@-sided dice from the tuple @(m,n)@.
fromTuple :: (Int, Int) -> Dice
fromTuple (dice, sides) = dice `d` sides


-- | Return the distribution
dist :: Dice -> RInt
dist ZeroDie = return 0
dist (MkDice dist' _ _) = dist'

-- | Number of dice
nDice :: Dice -> Int
nDice ZeroDie = 0
nDice (MkDice _ nDice' _) = nDice'

-- | Number of sides
nSides :: Dice -> Int
nSides ZeroDie = 0
nSides (MkDice _ _ nSides') = nSides'

-- | @m `d` n@ returns m n-sided dice as a 'Dice' value
d :: Int -> Int -> Dice
d number sides 
  | number > 0 && sides > 0 = MkDice diceDist number sides 
  | otherwise = ZeroDie
  where
  diceDist = sumRInts distlist
  distlist = replicate number $! R.pick $! (D.uniform [1..sides] :: Dist)

instance Eq Dice where
  ZeroDie == ZeroDie = True
  (MkDice _ nDiceX nSidesX) == (MkDice _ nDiceY nSidesY)
    = (nDiceX == nDiceY) && (nSidesX == nSidesY)
  _ == _ = False
 
instance Show Dice where
  show d = "fromTuple (" ++ show (nDice d) ++ "," ++ show (nSides d) ++ ")"

-- | Sum a number of random integers to yield a random integer.
sumRInts :: [RInt] -> RInt
sumRInts dists = foldr (liftM2 (+)) (return 0) dists

-- | Sum the random integers 
sumDice :: [Dice] -> RInt
sumDice dice = sumRInts (fmap dist dice)

-- | Add (a positive number of) extra dice. Sil mechanics never subtracts 
-- dice, only adds extra, so that's all we need to support.
addDice :: Int -> Dice -> Dice
addDice nExtraDice origDice 
  | nExtraDice > 0 = newNDice `d` (nSides origDice)
  | otherwise = origDice
  where
  newNDice = (nDice origDice) + nExtraDice
 
  
-- | The arithmetic mean of a 'Dist'.
mean :: Dist -> Float
mean d = D.expected $ fmap fromIntegral d

-- | The standard deviation of a 'Dist'.
std :: Dist -> Float
std d = D.stdDev $ fmap fromIntegral d

-- | The minimal element of a 'Dist'. (This is a total function because
-- a 'Dist' always has a finite set of values.)
minD :: Dist -> Int
minD = minimum . (map fst) . D.decons

-- | The maximal element of a 'Dist'. (This is a total function because
-- a 'Dist' always has a finite set of values.)
maxD :: Dist -> Int
maxD = maximum . (map fst) . D.decons

-- | @cdf interval d@ returns the cumulative distribution function for @d@
-- evaluated at @[minD d, minD d + interval .. maxD d]@.
cdf :: Int -> Dist -> [(Int, Float)]
cdf interval d =
  let minVal = minD d
      maxVal = maxD d
      values = [minVal,minVal+interval .. maxVal]
      probabilities = map (\v -> (< v) D.?? d) values
  in zip values probabilities

-- | The complementary cumulative distribution function.
ccdf :: Int -> Dist -> [(Int, Float)]
ccdf i d = map (\(x,p) -> (x,1-p)) $ cdf i d

-- | @printCDF decimals c@ prints @c@ (a cdf or ccdf) with all probabilities
-- output with @decimals@ decimal places; values with probabilities that are 0 
-- to @decimals@ decimal places will not be printed. If @decimals == 0@, all
-- values will be printed and no probabilities will be truncated.
printCDF :: Int -> [(Int, Float)] -> String 
printCDF decimals c = unlines $ map printPair $ filter aboveProbThres c
  where
  aboveProbThres = if decimals > 0 
                    then (>= 10 ** (- (fromIntegral decimals))) . snd
                    else (> 0) . snd
  printPair (x,p) = if decimals > 0
                      then show x ++ " " ++ showFFloat (Just decimals) p ""
                      else show x ++ " " ++ show p
