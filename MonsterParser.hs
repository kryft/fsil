module MonsterParser (parseMonsterFile) where

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import Monster
import Dice (Dice, d, zeroDie)
import GeneralParse

monsterFile :: Parser [Monster]
monsterFile = junk >> monsterEntry `sepEndBy` junk 

junk = anyChar `manyTill` (lookAhead nameFieldStart <|> myEof)

parseMonsterFile :: String -> IO [Monster]
parseMonsterFile filename = 
  do
    maybeMonsters <- parseFromFile monsterFile filename
    case maybeMonsters of
      Right monsters -> return monsters
      Left parseError -> error $ show parseError

nameFieldStart :: Parser String
nameFieldStart = try $
  do
      string "N:" 
      digits <- many1 digit 
      if digits == "0"
        then parserFail "not a monster"
        else return digits

monsterEntry :: Parser Monster
monsterEntry = try $
  do
    name <- nameField
    skipTillField "I" 
    speed <- speedField 
    skipTillField "P"
    (evasion,protDice) <- protectionField
    skipTillField "B"
    attacks <- many1 attackField
    anyChar `manyTill` try (eol >> eol)
    return $ Monster { name = name,
                       speed = speed,
                       evasion = evasion,
                       protDice = protDice,
                       attacks = attacks }


skipTillField fid = try $ do
  chars <- (lookAhead $ string (fid ++ ":"))
            <|> anyChar `manyTill` (lookAhead $ try (eol >> string (fid ++ ":")))
  optional eol --chomp eol if we found one in the look ahead 
  return chars

nameField :: Parser String
nameField = 
  do
    string "N:"
    many1 digit 
    char ':'
    name <- many1 (letter <|> oneOf " ,-'")
    eol
    return name


speedField :: Parser Int
speedField = 
  do
    string "I:" 
    speed <- parseInt
    anyChar `manyTill` eol 
    return speed

protectionField :: Parser (Int,Dice)
protectionField = 
  do
    string "P:"
    (ev,protDice) <- parseDefense
    spaces 
    return (ev, protDice)

attackField :: Parser (Int, Dice)
attackField = 
  do
    string "B:"
    anyChar `manyTill` (lookAhead $ char '(')
    (accuracy, damDice) <- parseAttack
    spaces
    return $ (accuracy, damDice)

