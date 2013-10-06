-- | Some simple parsers used by the other fsil parsers.
module GeneralParse where

import Rdice (Dice(ZeroDie), d)
import Numeric (readFloat)
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

--The parser 'space' also skips newlines, so this is useful in some situations
spaceBar :: Parser Char
spaceBar = char ' '

-- | Skips everything until either 'relevantBit' or 'endOfInput' succeeds.
--If 'relevantBit' succeeds, returns the result of that; otherwise
--fails.
ignoreUntil :: Parser a -> Parser b -> Parser a
ignoreUntil relevantBit endOfInput = try relevantBit 
  <|> (try endOfInput >> parserFail "parser 'stop' succeeded")
  <|> (anyChar >> ignoreUntil relevantBit endOfInput)

-- | Takes a list of parsers and a parser for a separator, and tries to
-- apply each of the list's parsers in turn, alternating with the separator 
-- parser. E.g. seqSepBy [string "a", string "b", string "c"] string "," would
-- correctly parse "a,b,c" but fail on "ab,c" or "a,,b,c".
seqSepBy :: [Parser a] -> Parser a -> Parser [a]
seqSepBy (p:[]) _ = do {pRes <- p; return [pRes]}
seqSepBy (p:ps) separator =
  do
    pRes <- p
    sep <- separator
    rest <- seqSepBy ps separator
    return $ pRes : sep : rest 
  
-- | Parses an integer with an optional sign.
parseInt :: Parser Int
parseInt = 
  do
    sign <- option '+' (char '+' <|> char '-')
    digits <- many1 digit
    let value = read digits
    return $ if sign == '+' then value else (-value)

-- | Parses a positive double in decimal notation.
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

    

-- | Parses dice in the format XdY, e.g. 12d6
diceParser :: Parser Dice
diceParser = 
  do
    nDice <- parseInt
    char 'd'
    nSides <- parseInt
    return $ nDice `d` nSides

-- | Parses an end of line character; should parse unix, dos or mac eol correctly.
eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

-- | Parses a Sil defense tuple, e.g. [-3], [1d1], [+5,2d5]
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
 
 -- | Parses a Sil attack tuple, e.g. (+3) or (+5,2d6)
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
