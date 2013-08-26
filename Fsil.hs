import Control.Monad
import Numeric
import Data.List ((!!))
import qualified Numeric.Probability.Distribution 
import qualified Monster as M
import qualified Player as P
import Types
import Dice
import MonsterParser --ghci convenience


monsBaseCritThres = 7


data FightStats = FightStats { damGiven :: Dist, 
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

--
attackDist :: [Attack] -> Int -> [Dice] -> Dist
attackDist attack evasion protectionDice =
  do
    toHit <- D.norm $ toHitDist (A.accuracy attack) evasion
    if toHit < 1 then
      D.certainly 0
    else do
      let damDice' = addDice nCritDice damDice
          nCritDice = nCrits baseCriticalThreshold toHit
      D.norm $ damageDist damDice' protectionDice


fight :: P.Player -> M.Monster -> FightStats
fight player monster =
  let monsAttacks = (M.attacks monster)
      damGiven' = attackDist player monster 
      oppBaseCritThres = monsBaseCritThres + 2 * fromIntegral (nDice oppDam)
      damTaken' = attackDist oppAcc oppDam oppBaseCritThres ev protDice
  in
    FightStats { damGiven = damGiven', damTaken = damTaken', 
                 player = "player", opponent = opponent' }

protModifier :: Sharpness -> Double
protModifier NotSharp = 1.0
protModifier Sharp = 0.5
protModifier VerySharp = 0


