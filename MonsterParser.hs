module MonsterParser (parseMonsterFile, junk, nameFieldStart) where

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import Monster
import Dice (Dice, d, zeroDie)

monsterFile :: Parser [Monster]
monsterFile = junk >> monsterEntry `sepEndBy` junk 

junk = anyChar `manyTill` (lookAhead nameFieldStart <|> do {lookAhead eof; return " "})

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
monsterEntry = 
  do
    name <- nameField
    skipTillField "I"
    speed <- speedField
    skipTillField "P"
    (evasion,protDice) <- protectionField
    skipTillField "B"
    attacks <- many1 attackField
    return $ Monster { monsName = name,
                       monsSpeed = speed,
                       monsEvasion = evasion,
                       monsProtDice = protDice,
                       monsAttacks = attacks }

skipTillField fid = do
  chars <- anyChar `manyTill` (lookAhead $ try (eol >> string (fid ++ ":")))
  eol
  return chars

nameField :: Parser String
nameField = 
  do
    string "N:"
    many1 digit 
    char ':'
    name <- many1 (letter <|> char ' ' <|> char ',' <|> char '-')
    eol
    return name


eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"


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
    char '['
    ev <- parseInt
    protDice <- option zeroDie $ try $ do
      char ','
      diceParser
    char ']'
    spaces 
    return (ev, protDice)

attackField :: Parser (Int, Dice)
attackField = 
  do
    string "B:"
    anyChar `manyTill` (char '(')
    accuracy <- parseInt
    damDice <- option zeroDie $ try $ do
      char ','
      diceParser
    char ')' 
    spaces
    return $ (accuracy, damDice)

parseInt :: Parser Int
parseInt = 
  do
    sign <- option '+' (char '+' <|> char '-')
    digits <- many1 digit
    let value = read digits
    return $ if sign == '+' then value else (-value)

diceParser :: Parser Dice
diceParser = 
  do
    nDice <- parseInt
    char 'd'
    nSides <- parseInt
    return $ nDice `d` nSides



