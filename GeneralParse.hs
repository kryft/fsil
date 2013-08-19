module GeneralParse where

import Dice (Dice, d, zeroDie)
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)


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

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

myEof :: Parser String
myEof = do {eof; return " "}

parseDefense :: Parser (Int, Dice)
parseDefense =
  do
    char '['
    ev <- parseInt
    protDice <- option zeroDie $ try $ do
      char ','
      diceParser
    char ']'
    return (ev, protDice)
 
parseAttack :: Parser (Int, Dice)
parseAttack = 
 do
    char '('
    accuracy <- parseInt
    damDice <- option zeroDie $ try $ do
      char ','
      diceParser
    char ')' 
    spaces
    return $ (accuracy, damDice)
