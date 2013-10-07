module Monster where

import qualified Data.Map as Map
import Rdice
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

-- | 'Monster' represents a Sil monster, containing all the information
-- we need to simulate attacks by or against the monster. Many of the
-- fields are identical in name and function to their 'Player' 
-- counterparts.
data Monster = Monster { name :: String, 
                         depth :: Int, 
                         -- ^ the depth at which a monster is usually found;
                         -- doesn't affect any combat calculations,  but can 
                         -- be interesting for 
                         attacks :: [Attack], 
                         evasion :: Int,
                         lightRadius :: Int,
                         alertness :: Alertness,
                         protDice :: Dice,
                         speed :: Int,
                         -- ^ The monster's speed, between 1 and 4. Currently
                         -- not used by fsil.
                         will :: Int,
                         health :: Dice,
                         -- ^ The dice rolled to determine the monster's max
                         -- hitpoints. 
                         hatesLight :: Bool,
                         -- ^Some monsters are vulnerable to strong light.
                         glows :: Bool,
                         -- ^Balrogs glow, meaning that they're always 
                         -- visible.
                         seenByPlayer :: Bool, 
                         -- Fsil doesn't currently do perception checks for
                         -- whether the player can see an invisible monster
                         -- or not; if SeenByPlayer is True, the player can
                         -- always see the monster (if it's not too dark),
                         -- and if SeenByPlayer is False, the monster will
                         -- be invisible regardless of light. Defaults to
                         -- True for all monsters.  
                         resistances :: Map.Map Element Int,
                         criticalRes :: MonsterCritRes,
                         slainBy :: Maybe Slay,
                         -- ^ SlayOrcs if the monster is an orc, etc;
                         -- Nothing if there is no slay for this monster
                         -- in the game.
                         onLitSquare :: Bool
                       } deriving Show


dungeonLight m = if onLitSquare m then 1 else 0

-- | Check whether a monster's name matches a regular expression (given as
-- a string)
matchMonster :: String -> Monster -> Bool
matchMonster regex mons = (name mons) =~ regex

baseCritThreshold = 7.0

-- | The base critical threshold of a monster is determined by it's damage
-- dice.
monsterCritThreshold :: Dice -> Double
monsterCritThreshold dice = baseCritThreshold + 2 * fromIntegral (nDice dice)

modifyEvasionWith :: (Int -> Int) -> Monster -> Monster 
modifyEvasionWith func monster = monster {evasion = func $ evasion monster}

modifyAccuracyWith :: (Int -> Int) -> Monster -> Monster
modifyAccuracyWith func monster =
  let newAttacks = Prelude.map (adjustAccuracy func) (attacks monster)
      adjustAccuracy func attack = attack { accuracy = func $ accuracy attack}
  in monster {attacks = newAttacks}

modifyCritThreshold :: (Double -> Double) -> Monster -> Monster
modifyCritThreshold func monster = 
  let newAttacks = Prelude.map (adjustCritThres func) (attacks monster)
      adjustCritThres f a = a { critThreshold = f $ critThreshold a }
  in monster {attacks = newAttacks}



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
