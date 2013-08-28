module GeneralParse where

import Dice (Dice(ZeroDie), d)
import Numeric (readFloat)
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

--The parser 'space' also skips newlines, so this is useful in some situations
spaceBar :: Parser Char
spaceBar = char ' '

--Skips everything until either 'relevantBit' or 'endOfInput' succeeds.
--If 'relevantBit' succeeds, returns the result of that; otherwise
--fails.
ignoreUntil :: Parser a -> Parser b -> Parser a
ignoreUntil relevantBit endOfInput = try relevantBit 
  <|> (try endOfInput >> parserFail "parser 'stop' succeeded")
  <|> (anyChar >> ignoreUntil relevantBit endOfInput)

seqSepBy :: [Parser a] -> Parser a -> Parser [a]
seqSepBy (p:[]) _ = do {pRes <- p; return [pRes]}
seqSepBy (p:ps) separator =
  do
    pRes <- p
    sep <- separator
    rest <- seqSepBy ps separator
    return $ pRes : sep : rest 
  

parseInt :: Parser Int
parseInt = 
  do
    sign <- option '+' (char '+' <|> char '-')
    digits <- many1 digit
    let value = read digits
    return $ if sign == '+' then value else (-value)

parseDouble :: Parser Double
parseDouble = try $
  do
    beforeRadix <- many1 digit
    radix <- char '.'
    afterRadix <- many1 digit
    let parsedFloat = readFloat $ beforeRadix ++ [radix] ++ afterRadix
    case parsedFloat of
      [(f,_)] -> return f
      _ -> parserFail "failed parsing double"

    


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

parseDefenseTuple :: Parser (Int, Dice)
parseDefenseTuple =
  do
    char '['
    ev <- option 0 $ try $ do 
      i <- parseInt
      notFollowedBy $ char 'd'
      return i
    protDice <- option ZeroDie $ try $ do
      optional $ char ','
      diceParser
    char ']'
    return (ev, protDice)
 
parseAttackTuple :: Parser (Int, Dice)
parseAttackTuple = 
 do
    char '('
    accuracy <- parseInt
    damDice <- option ZeroDie $ try $ do
      char ','
      diceParser
    char ')' 
    spaces
    return $ (accuracy, damDice)
