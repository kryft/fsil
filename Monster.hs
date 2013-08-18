module Monster where

import Dice

data Monster = Monster { monsName :: String, 
                         monsAttacks :: [(Int,Dice)], 
                         monsEvasion :: Int,
                         monsProtDice :: Dice,
                         monsSpeed :: Int
              } deriving Show

