import Control.Monad
import Control.Monad.State
import Numeric
import Data.List (concat)
import qualified Numeric.Probability.Distribution as D
import qualified Monster as M
import qualified Player as P
import qualified Data.Map as Map
import qualified Types as T
import Dice
import qualified CombatState as CS
import MonsterParser --ghci convenience
import CharDumpParser --ghci convenience


data FightStats = FightStats { damGiven :: Dist, 
                               damTaken :: [Dist], 
                               player :: P.Player, 
                               opponent :: M.Monster
                             }


summarize fs = (P.name (player fs)) ++ " vs " ++ (M.name (opponent fs)) 
  ++ "; Mean damage taken: " ++ show (map mean (damTaken fs)) 
  ++ "; Mean damage dealt: " ++ show ( mean (damGiven fs))

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

attackDist :: T.Attack -> Int -> [Dice] -> Dist
attackDist attack evasion protDice =
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

attackSeqDist :: [T.Attack] -> Int -> [Dice] -> Dist
attackSeqDist [] _ _ = return 0
attackSeqDist (a:as) evasion protDice =
  do
    damRest <- attackSeqDist as evasion protDice
    dam <- attackDist a evasion protDice
    D.norm $ return $ dam + damRest

fight :: P.Player -> M.Monster -> FightStats
fight player monster =
  let (_,(p,m)) = runState CS.applyCombatModifiers (player,monster)
      pAttacks = P.attacks p
      pEv = P.evasion p
      pProt = P.protDice p
      mAttacks = M.attacks m
      mEv = M.evasion m
      mProt = M.protDice m
      damGiven = attackSeqDist pAttacks mEv [mProt]
      --When a monster has several attacks, they're mutually exclusive
      --alternatives, so compute separate distributions for each
      damTaken = map distForOneAttack mAttacks
      distForOneAttack a = attackDist a pEv pProt
  in
      FightStats { damGiven = damGiven, 
                 damTaken = damTaken, 
                 player = p, 
                 opponent = m}

