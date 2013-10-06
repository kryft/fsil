module MonsterParser (parseMonsterFile) where

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import qualified Monster as M
import Rdice (Dice(ZeroDie), d)
import GeneralParse
import Types as T
import qualified Data.Map as Map

monsterFile :: Parser [M.Monster]
monsterFile = junk >> monsterEntry `sepEndBy` junk 

junk = anyChar `manyTill` (lookAhead nameFieldStart <|> (eof >> return " "))

parseMonsterFile :: String -> IO [M.Monster]
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

monsterEntry :: Parser M.Monster
monsterEntry = try $
  do
    name <- nameField
    spaces
    depth <- depthField
    skipTillField "I" 
    (speed,health,lightRadius) <- infoField 
    --Field A: always comes after I:
    spaces
    will <- alertnessField
    skipTillField "P"
    (evasion,protDice) <- protectionField
    --If the attack field B: exists (as it does for every monster
    --except silent watchers), it always comes after P:
    spaces
    attacks <- option [] $ do
      many1 $ do 
        notFollowedBy $ string "F:"
        attack <- attackField 
        spaces 
        return attack

    (critRes,resistances,hatesLight,slainBy,glows) <- flagsField
    anyChar `manyTill` try endOfMonsterEntry
    return $ M.Monster { M.name = name,
                       M.depth = depth,
                       M.speed = speed,
                       M.will = will,
                       M.evasion = evasion,
                       M.protDice = protDice,
                       M.attacks = attacks,
                       M.lightRadius = lightRadius,
                       M.hatesLight = hatesLight,
                       M.health = health,
                       M.glows = glows,
                       M.seenByPlayer = True,
                       M.resistances = resistances,
                       M.criticalRes = critRes,
                       M.slainBy = slainBy,
                       M.onLitSquare = False,
                       M.alertness = M.Alert}



endOfMonsterEntry = try $ eol >> eol

endOfField = many spaceBar >> eol

endOfSubField = try (string ":") <|> endOfField

skipTillField fid = ignoreUntil fieldStart endOfMonsterEntry >> eol
  where fieldStart = lookAhead $ eol >> string (fid ++ ":")


lookForField fid = try $ lookAhead $ 
  (notFollowedBy endOfMonsterEntry >> anyChar) `manyTill` fieldStart
  where 
    fieldStart = try $ eol >> string (fid ++ ":")


anySubField :: Parser String
anySubField = anyChar `manyTill` lookAhead endOfSubField 

subField :: Parser a -> Parser a
subField parser = try $ do
  contents <- parser
  --If necessary, skip to the beginning of the next subfield
  --or the end of this field
  anyChar `manyTill` endOfSubField 
  return contents


ignoreSubField = subField anySubField


nameField :: Parser String
nameField = try $
  do
    string "N:"
    ignoreSubField -- serial number; ignored for now
    subField $ many1 (letter <|> oneOf " ,-'") --name of monster

depthField :: Parser Int
depthField = try $
  do
    string "W:"
    parseInt --monster depth

infoField :: Parser (Int, Dice, Int)
infoField = try $
  do
    string "I:" 
    speed <- subField parseInt
    health <- subField diceParser
    ignoreSubField
    lightRadius <- subField parseInt
    return (speed,health,lightRadius)

alertnessField :: Parser Int
alertnessField = try $
  do
    string "A:"
    count 3 ignoreSubField
    parseInt --Will score



--The P: field is for evasion and protection dice
protectionField :: Parser (Int,Dice)
protectionField = 
  do
    string "P:"
    (ev,protDice) <- parseDefenseTuple
    return (ev, protDice)

attackField :: Parser Attack
attackField = 
  do
    string "B:"
    (brands, sharpness, canCrit, alwaysHits) <- parseAttackEffects
    (accuracy, damDice) <- parseAttackTuple
    return $ Attack {accuracy = accuracy,
                     damage = damDice,
                     brands = brands,
                     slays = [],
                     sharpness = sharpness,
                     critThreshold = M.monsterCritThreshold damDice,
                     canCrit = canCrit,
                     alwaysHits = alwaysHits}

parseAttackEffects :: Parser ([Element], Double, Bool, Bool)
parseAttackEffects =
  do
    (alwaysHits,sharpness,canCrit) <- subField $ 
      (try (string "CRAWL") >> return (False,0.5, True)) <|>
      (try (string "ENGULF") >> return (False,1.0, False)) <|>
      (try (string "TOUCH") >> return (False,0.0, False)) <|>
      (try (string "SPORE") >> return (True,0.0, False)) <|>
      (anySubField >> return (False,1.0,True))
    brands <- option [] (try $ subField $ 
      (try (string "FIRE") >> return [Fire]) <|>
      (try (string "COLD") >> return [Cold]) <|>
      (try (string "POISON") >> return [Poison]) <|>
      (try (string "DARK") >> return [Dark]) <|>
      --If the field starts with a '(', then this monster didn't have
      --an attack effect field, and this field is in fact the attackTuple
      --field parsed in the next step of attackField
      (notFollowedBy (char '(') >> anySubField >> return []))
    return (brands, sharpness, canCrit, alwaysHits)


flagsField :: Parser (M.MonsterCritRes, Map.Map Element Int, Bool, Maybe Slay, Bool)
flagsField = do
  string "F:" <?> "start of flags field"
  flags <- flag `sepBy` try flagSep 
  let critRes = if "NO_CRIT" `elem` flags then M.CritImmune
                  else if  "RES_CRIT" `elem` flags then M.CritResistant
                       else M.NoCritRes
      resList = resistancesFromFlags flags
      vulnList = vulnerabilitiesFromFlags flags
      resMap = T.makeResistanceMap resList vulnList
      maybeSlainBy = slayFromFlags flags
      hatesLight = "HURT_LITE" `elem` flags
      glows = "GLOW" `elem` flags
  return (critRes, resMap, hatesLight, maybeSlainBy, glows)
    where
    flag = many1 (upper <|> digit <|> char '_')
    flagSep = try (spaces >> char '|' >> spaces >> return "|") <|> 
      try (many spaceBar >> eol >> string "F:")

resistancesFromFlags [] = []
resistancesFromFlags (f:fs) = 
  case f of
    "RES_FIRE" -> Fire : resistancesFromFlags fs
    "RES_COLD" -> Cold : resistancesFromFlags fs
    "RES_POIS" -> Poison : resistancesFromFlags fs
    _ -> resistancesFromFlags fs

vulnerabilitiesFromFlags [] = []
vulnerabilitiesFromFlags (f:fs) = 
  case f of
    "HURT_FIRE" -> Fire : vulnerabilitiesFromFlags fs
    "HURT_COLD" -> Cold : vulnerabilitiesFromFlags fs
    _ -> vulnerabilitiesFromFlags fs


slayFromFlags [] = Nothing
slayFromFlags (f:fs) =
  case f of
    "WOLF" -> Just SlayWolves
    "ORC" -> Just SlayOrcs
    "UNDEAD" -> Just SlayUndead
    "RAUKO" -> Just SlayRaukar
    "TROLL" -> Just SlayTrolls
    "DRAGON" -> Just SlayDragons
    "SPIDER" -> Just SlaySpiders
    _ -> slayFromFlags fs

