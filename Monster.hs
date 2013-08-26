module Monster where

import Data.Map as Map
import Dice
import Text.Regex.Posix ((=~))
import Attack

data MonsterCritRes = None | Resistant | Immune

data Alertness = Alert | Unwary | Sleeping

--seenByPlayer defaults to true (Fsil doesn't do perception checks for
--Sulrauko etc); use this to make the monster invisible
unseen :: Monster -> Monster
unseen m = m {seenByPlayer = False}

sleeping :: Monster -> Monster
sleeping m = m { alertness = Sleeping }
--sleeping m = m {evasion = -5}

unwary :: Monster -> Monster
unwary m = m {alertness = Unwary}
--unwary m = m {evasion = truncate $ (evasion m) / 2}

data Monster = Monster { name :: String, 
                         attacks :: [Attack], 
                         evasion :: Int,
                         lightRadius :: Int,
                         alertness :: Alertness,
                         protDice :: Dice,
                         speed :: Int,
                         hatesLight :: Bool,
                         seenByPlayer :: Bool, 
                         resistances :: Map.Map Element Int,
                         criticalRes :: MonsterCritRes,
                         slainby :: Slay,
                         onLitSquare :: Bool
                       } deriving Show



matchMonster :: String -> Monster -> Bool
matchMonster regex mons = (monsName mons) =~ regex

getMonster :: String -> [Monster] -> Monster
getMonster nameRegex monsList = 
  let matchingMonsters = filter (matchMonster nameRegex) monsList
  in if null matchingMonsters
        then head monsList
        else head matchingMonsters
