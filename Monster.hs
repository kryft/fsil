module Monster where

import Dice
import Data.List (null, filter)
import Text.Regex.Posix ((=~))

data Monster = Monster { monsName :: String, 
                         monsAttacks :: [(Int,Dice)], 
                         monsEvasion :: Int,
                         monsProtDice :: Dice,
                         monsSpeed :: Int
                       } deriving Show


matchMonster :: String -> Monster -> Bool
matchMonster regex mons = (monsName mons) =~ regex

getMonster :: String -> [Monster] -> Monster
getMonster nameRegex monsList = 
  let matchingMonsters = filter (matchMonster nameRegex) monsList
  in if null matchingMonsters
        then head monsList
        else head matchingMonsters
