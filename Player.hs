module Player where

import Types as T
import Rdice
import qualified Data.Map as Map

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
                       resistances = T.noResistance,
                       abilities = [],
                       equipment = [],
                       will = 0,
                       song = 0,
                       stealth = 0,
                       onLitSquare = False }


hasEmptyOffHand :: Player -> Bool
hasEmptyOffHand p = (T.eqName offHandItem) == "(nothing)"
  where 
    offHandItem = head $ filter isOffHand (equipment p)
    isOffHand x = (T.eqSlot x) == T.OffHand

singing :: Singing -> Player -> Player
singing singing' p = p {activeSongs = singing'}

withAbilities :: [T.Ability] -> Player -> Player
withAbilities as p = p {abilities = (abilities p) ++ as}

isActiveSong :: Song -> Player -> Bool
isActiveSong song' player =
  case (activeSongs player) of 
    Quiet -> False
    OneSong s -> song' == s
    WovenThemes (s1, s2) -> (s1 == song' || s2 == song') 

hasAbility :: Player -> Ability -> Bool
hasAbility p a = a `elem` (abilities p)

--Light strength on player's square (not including effects from
--the environment or monsters)
playerLight :: Player -> Int
playerLight player = (lightRadius player) + 1 + innerLight 
  where
    innerLight = if InnerLight `elem` (abilities player)
                  then 1 else 0

applySongTrees :: Player -> Player
applySongTrees p =
  let extraLight = (getSongValue SongTrees p) `quot` 5
      light = lightRadius p
  in p {lightRadius = light + extraLight}

applySongStay :: Player -> Player
applySongStay p =
  let extraProtSides = (getSongValue SongStay p) `quot` 3
      newProt = (1 `d` extraProtSides) : (protDice p)
      extraWill = extraProtSides
      newWill = (will p) + extraWill
  in p {protDice = newProt, will = newWill}

applySongSharp :: Player -> Player
applySongSharp p =
  let sharpnessBonus = min 6 $ getSongValue SongSharp p
      extraSharpness = 6.0 / (fromIntegral sharpnessBonus)
      newAttacks = map multiplySharpness (attacks p) :: [Attack]
      multiplySharpness a = 
        a {T.sharpness = (T.sharpness a) * extraSharpness}
  in p {attacks = newAttacks}


dungeonLight :: Player -> Int
dungeonLight p = if (onLitSquare p) then 1 else 0

modifyEvasionWith :: (Int -> Int) -> Player -> Player 
modifyEvasionWith func player = player {evasion = func $ evasion player}

modifyAccuracyWith :: (Int -> Int) -> Player -> Player
modifyAccuracyWith func player =
  let newAttacks = Prelude.map (adjustAccuracy func) (attacks player)
      adjustAccuracy func attack = attack { accuracy = func $ accuracy attack}
  in player {attacks = newAttacks}

modifyCritThreshold :: (Double -> Double) -> Player -> Player
modifyCritThreshold func player = 
  let newAttacks = Prelude.map (adjustCritThres func) (attacks player)
      adjustCritThres f a = a { critThreshold = f $ critThreshold a }
  in player {attacks = newAttacks}


     

--Returns the effective song strength for the given Song:
--the player's song score if the player is currently singing that song
--(or song score - 5 if the song is a minor theme) and 0 otherwise.
getSongValue :: Song -> Player -> Int
getSongValue song' player =
  case (activeSongs player) of 
    Quiet -> 0
    OneSong s -> 
      if song' == s then (song player) else 0
    WovenThemes (s1, s2) 
      | s1 == song' -> max 0 $ song player
      | s2 == song' -> max 0 $ (song player) - 5
      | otherwise -> 0

playerCritMods :: Player -> Double
playerCritMods player =
  let finesse = if player `hasAbility` T.Finesse
                  then -1.0
                  else 0
      subtlety = if player `hasAbility` T.Subtlety 
                    && hasEmptyOffHand player
                   then -2.0
                   else 0
      power = if player `hasAbility` T.Power
                then 1.0
                else 0
  in finesse + subtlety + power

