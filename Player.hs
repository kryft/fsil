module Player where

import Types
import Dice
import Data.Map as Map

--The base number by which the player's to-hit roll needs to exceed
--a monster's evasion roll for a critical hit to occur.
playerBaseCritThres = 7.0

data Player = Player { name :: String,
                       attacks :: [Attack],
                       evasion :: Int,
                       protDice :: [Dice],
                       lightRadius :: Int,
                       activeSongs :: Singing,
                       resistances :: Map.Map Element Int,
                       abilities :: [Ability],
                       equipment :: [Equipment],
                       will :: Int,
                       song :: Int,
                       stealth :: Int,
                       onLitSquare :: Bool
                     } deriving (Show)

blankPlayer = Player { name = "",
                       attacks = [],
                       evasion = 0,
                       protDice = [],
                       lightRadius = 0,
                       activeSongs = Quiet,
                       resistances = noResistance,
                       abilities = [],
                       equipment = [],
                       will = 0,
                       song = 0,
                       stealth = 0,
                       onLitSquare = False }

noResistance = fromList [(Fire,0), (Cold,0), (Poison,0), (Lightning,0), 
  (Dark,0)]

singing :: Singing -> Player -> Player
singing singing' p = p {activeSongs = singing'}

isActiveSong :: Song -> Player -> Bool
isActiveSong song' player =
  case (activeSongs player) of 
    Quiet -> False
    OneSong s -> song' == s
    WovenThemes (s1, s2) -> (s1 == song' || s2 == song') 


--Returns the effective song strength for the given Song:
--the player's song score if the player is currently singing that song
--(or song score - 5 if the song is a minor theme) and 0 otherwise.
--getSongValue :: Song -> Player -> Int
--getSongValue song' player =
--  case (activeSongs player) of 
--    Quiet -> 0
--    OneSong s -> 
--      if song' == s then (song player) else 0
--    WovenThemes (s1, s2) 
--      | s1 == song' -> song player
--      | s2 == song' -> (song player) - 5
--      | otherwise -> 0

modifyEvasionWith :: (Int -> Int) -> Player -> Player 
modifyEvasionWith func player = player {evasion = func $ evasion player}

modifyAccuracyWith :: (Int -> Int) -> Player -> Player
modifyAccuracyWith func player =
  let newAttacks = Prelude.map (adjustAccuracy func) (attacks player)
      adjustAccuracy func attack = attack { accuracy = func $ accuracy attack}
  in player {attacks = newAttacks}

playerCritThres :: [Ability] -> Double -> Double
playerCritThres abilities weaponWeight =
  let base = playerBaseCritThres + weaponWeight
      finesse = if Finesse `elem` abilities
                  then -1.0
                  else 0
      subtlety = if Subtlety `elem` abilities
                   then -2.0
                   else 0
      power = if Power `elem` abilities
                then 1.0
                else 0
  in base + finesse + subtlety + power

