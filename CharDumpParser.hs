module CharDumpParser where

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import qualified Player as P
import Data.List (elem)
import Dice (Dice, d, zeroDie)

charDumpFile :: Parser P.Player
charDumpFile filename = 
  do
    skipTill "Name"
    name <- parseName
    skipTill "Melee"
    attacks <- parseAttacks
    skipTill "Armor"
    ev <- parseEv
    skipTill "[Equipment]"
    protDice <- parseProtEquip
    weaponWeights <- parseWeaponWeights
    skipTill "[Notes]"
    abilities <- parseAbilities
    let critThresholds = map (playerCritThres abilities) weaponWeights
    return P.Player 
      { P.name = name,
        P.attacks = attacks,
        P.evasion = ev,
        P.protDice = protDice,
        P.abilities = abilities,
        P.dumpFileName = filename,
        P.critThresholds = critThresholds
      }





playerCritThres abilities weaponWeight =
  let base = P.PlayerBaseCritThres + weaponWeight
      finesse = if "Finesse" `elem` abilities
                  then -1.0
                  else 0
      subtlety = if "Subtlety" `elem` abilities
                   then -2.0
                   else 0
      power = if "Power" `elem` abilities
                then 1.0
                else 0
  in base + finesse + subtlety + power


