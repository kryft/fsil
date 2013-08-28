module Monster where

import qualified Data.Map as Map
import Dice
import Types
import Text.Regex.Posix ((=~))
--import Data.List(filter,null)

data MonsterCritRes = NoCritRes | CritResistant | CritImmune
  deriving (Show,Eq)

data Alertness = Alert | Unwary | Sleeping
  deriving (Show,Eq)

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
                         slainBy :: Maybe Slay,
                         onLitSquare :: Bool
                       } deriving Show



matchMonster :: String -> Monster -> Bool
matchMonster regex mons = (name mons) =~ regex

baseCritThreshold = 7.0

monsterCritThreshold :: Dice -> Double
monsterCritThreshold dice = baseCritThreshold + 2 * fromIntegral (nDice dice)


--Finds the first monster in monsList whose name matches nameRegex.
--Gives preference to monsters whose name starts with regex, so 
--getMonster "Morgoth" will return the monster "Morgoth, Lord of Darkness"
--rather than "Gorthaur, servant of Morgoth". 
--If no monster's name matches nameRegex, returns the first monster of
--monsList.
getMonster :: String -> [Monster] -> Monster
getMonster nameRegex monsList = 
  let startsWithName = '^' : nameRegex
      matchingMonsters = filter (matchMonster startsWithName) monsList
      matchingMonsters' = filter (matchMonster nameRegex) monsList
  in if null matchingMonsters
        then if null matchingMonsters'
             then head monsList --default Orc scout :P
             else head matchingMonsters'
        else head matchingMonsters
