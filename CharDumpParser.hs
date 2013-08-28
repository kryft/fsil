module CharDumpParser where

import Text.Parsec
import Data.Char(isSpace)
import Text.Parsec.String (Parser, parseFromFile)
import qualified Player as P
import Data.List (concat, group)
import Data.Maybe (catMaybes, fromJust, isJust)
import qualified Data.Map as Map
import Dice (Dice(ZeroDie), d)
import GeneralParse
import Types as T


readCharDump :: String -> IO P.Player
readCharDump filename = 
  do
    maybePlayer <- parseFromFile charDumpFile filename
    case maybePlayer of
      Right player -> return player
      Left parseError -> error $ show parseError

--Parses (or rather scrapes) a Sil character dump file.
charDumpFile :: Parser P.Player
charDumpFile = 
  do
    skipTill "Name"
    name <- parseName
    skipTill "Melee"
    --Use lookAhead here to ensure that we don't skip any of the
    --stats (Evasion etc) that we want to parse after this
    attackTuples <- lookAhead $ parseAttackTuples 
    evasion <- getStat "Evasion" 
    stealth <- getStat "Stealth"
    will <- getStat "Will"
    lightRadius <- getStat "Light Radius"
    song <- getStat "Song"
    skipTill "[Equipment]"
    equipment <- parseEquipment
    skipTill "[Notes]"
    abilities <- parseAbilities
    let protDice = 
          (filter (/= ZeroDie)) . catMaybes $ map eqProtDice equipment
        eqAbilities' = concat $ map eqAbilities equipment
        allAbilities = abilities ++ eqAbilities'
        attacks = makeAttacks attackTuples allAbilities equipment
    return $ P.Player 
      { P.name = name,
        P.attacks = attacks,
        P.lightRadius = lightRadius,
        P.evasion = evasion,
        P.stealth = stealth,
        P.resistances = resistancesFrom equipment,
        P.will = will,
        P.song = song,
        P.activeSongs = Quiet,
        P.equipment = equipment,
        P.protDice = protDice,
        P.abilities = abilities,
        P.onLitSquare = False
      }

skipTill str = anyChar `manyTill` (try $ string str)

getStat str = skipTill str >> spaces >> parseInt

doubleEol = eol >> eol

endItemDesc = try $ many spaceBar >> 
  (try (eol >> lookAhead slotID) <|> try doubleEol)


spaceOrSingleNewline = spaceBar <|> try ( eol >> notFollowedBy eol >> return '\n')



--There appear to be no restrictions on character names, so the only way 
--to parse the name is to read the entire field, whose max width is 13
parseName = try $ do
  spaces 
  nameField <- 13 `count` anyChar
  return $ stripSpacesAtEnd nameField

stripSpacesAtEnd = reverse . (dropWhile isSpace) . reverse

parseAttackTuples :: Parser [(Int,Dice)]
parseAttackTuples = try $ do
  spaces 
  (try parseAttackTuple) `endBy1` junk
  where 
    junk = anyChar `manyTill` lookAhead (string "(" <|> string "Bows")

parseEquipment :: Parser [Equipment]
parseEquipment = do
  anyChar `manyTill` lookAhead slotID
  equippedItem `sepEndBy` endItemDesc

slotID :: Parser String
slotID = try $ oneOf "abcdefghijklmn" >>= \id -> char ')' >> return [id, ')']

equippedItem :: Parser Equipment
equippedItem = do
  slotStr <- slotID
  let slotType = case slotStr of
                    "a)" -> MainHand
                    "i)" -> OffHand
                    _    -> OtherSlot
  name <- do
    spaces  
    (try $ string "(nothing)") <|>
      (anyChar `manyTill` (lookAhead (oneOf "([<") <|> (eol >> return ' ')))

  --Skip possible attack tuple
  optional $ between (char '(') (char ')') (many $ noneOf ")")
  many spaceBar
  --Just about any equipped item may grant protection
  maybeProtDice <- option Nothing (fmap (Just . snd) parseDefenseTuple) 
  --Skip possible stat bonus 
  optional $ many spaceBar >> between (char '<') (char '>') (many $ noneOf ">")
  many spaceBar
  --Only weapons have weight listed here (and we don't care about
  --weight for other items anyway)
  maybeWeight <- option Nothing $ fmap Just parseWeight
  let isMeleeWeapon = isJust maybeWeight && slotType /= OtherSlot
  resistances <- lookAhead $ parseResistances
  vulnerabilities <- lookAhead $ parseVulnerabilities
  abilities <- lookAhead $ parseItemAbilities
  --Only look for slays, brands and sharpness if it's a weapon
  brands <- if isMeleeWeapon then lookAhead parseBrands else return []
  slays <- if isMeleeWeapon then lookAhead parseSlays else return []
  sharpness <- if isMeleeWeapon then lookAhead parseSharpness 
                                else return NotSharp
  anyChar `manyTill` (lookAhead endItemDesc)

  return $ Equipment { eqName = stripSpacesAtEnd $ name,
                       eqSlot = slotType,
                       eqIsMeleeWeapon = isMeleeWeapon,
                       eqProtDice = maybeProtDice,
                       eqWeight = maybeWeight,
                       eqResistances = resistances,
                       eqSharpness = sharpness,
                       eqAbilities = abilities,
                       eqVulnerabilities = vulnerabilities,
                       eqBrands = brands,
                       eqSlays = slays }

 
parseWeight :: Parser Double
parseWeight = try $ do
  spaces
  weight <- parseDouble
  spaces
  string "lb"
  return weight


--Scrapes an item description in the [Equipment] section of a Sil char dump.
--By 'item description' I mean the lines below the 'name line', e.g. the last
--three lines below:
--a) The Broken Sword of Amras (+1,1d5) <+2> 1.0 lb
--   It improves your stealth and perception by 2.  It is branded 
--   with flame.  It provides resistance to fire.  It cannot be 
--   harmed by the elements.                            
--
--Skips input until relevantSentence parses successfully and returns the
--result of relevantSentence. Consumes no input if relevantSentence
--fails.

itemDescScraper :: Parser a -> Parser a
itemDescScraper relevantSentence = try $ ignoreUntil relevantSentence endItemDesc

--Parses a sentence that ends with a comma-separated list of 'properties',
--e.g. "It provides resistance to cold, fire and poison." sentenceStart
--should parse the 'preamble' ("It provides resistance to "), and 
--itemParser should parse individual items ("cold", "fire", "poison").
--Items that are not parsed by itemParser will be skipped (so it's
--possible that an empty list will be returned.)
propertyListSentence :: Parser String -> Parser a -> Parser [a]
propertyListSentence sentenceStart itemParser = do
  many spaceOrSingleNewline
  sentenceStart
  many spaceOrSingleNewline
  commaSepListOf itemParser

   
--Parses a list of items separated by commas or 'and'. Returns a list
--of all the items parsed by parseItem; if parseItem doesn't recognize
--an item string, that string is skipped.
commaSepListOf :: Parser a -> Parser [a]
commaSepListOf parseItem = do
  maybeItems <- maybeItem `sepBy` separator
  char '.'
  return $ catMaybes maybeItems
  where
    separator = (try $ string ", ") <|> (try $ string " and ") 
        <|> (try $  string ", and")
    maybeItem = justItem <|> unrecognizedItem 
    justItem = fmap Just parseItem
    unrecognizedItem = 
      do {anyChar `manyTill` (separator <|> string "."); return Nothing}
      

parseResistances :: Parser [Element]
parseResistances = option [] $ itemDescScraper resistanceSentence
  where resistanceSentence = try (propertyListSentence (mlString "It provides resistance to") parseElement)

parseVulnerabilities :: Parser [Element]
parseVulnerabilities = option [] $ itemDescScraper vulnerabilitySentence
  where vulnerabilitySentence =  try $ propertyListSentence (mlString "It makes you more vulnerable to") parseElement

parseBrands :: Parser [Element]
parseBrands = option [] $ itemDescScraper brandSentence
  where brandSentence = try $ propertyListSentence (mlString "It is branded with") parseElement

parseSlays :: Parser [Slay]
parseSlays = option [] $ itemDescScraper slaySentence
  where slaySentence = try $ propertyListSentence (mlString "It slays") parseSlay

parseItemAbilities :: Parser [Ability]
parseItemAbilities = option [] $ itemDescScraper abilitySentence
  where
    abilitySentence = propertyListSentence abilStart parseAbility
    abilStart = mlString "It grants you the ability:" <|>
                mlString "It grants you the abilities:"

parseSharpness = option NotSharp $ itemDescScraper sharpSentence 
  where sharpSentence = (mlString "cuts easily through armour" >> (return Sharp)) <|> (mlString "cuts very easily through armour" >> (return VerySharp))


--mlString parses str, but allows it to span multiple lines.
mlString :: String -> Parser String
mlString str = fmap concat $ (map string $ words str) `seqSepBy` separator
  where 
    separator = many spaceOrSingleNewline >> return " "


parseElement :: Parser Element
parseElement = do
  element <- try (string "flame") <|>
             try (string "frost") <|>
             try (string "fire") <|>
             try (string "cold") <|>
             try (string "poison") <|>
             try (string "lightning")
  return $ stringToElement element
  where 
    stringToElement str =
      case str of
        "flame" -> Fire
        "fire" -> Fire
        "frost" -> Cold
        "cold" -> Cold
        "poison" -> Poison
        "lightning" -> Lightning

parseSlay :: Parser Slay
parseSlay = do
  slay <- try (string "orcs") <|>
          try (string "trolls") <|>
          try (string "wolves") <|>
          try (string "spiders") <|>
          try (string "undead") <|>
          try (string "dragons") <|>
          try (string "raukar") 
  return $ stringToSlay slay
  where
    stringToSlay slay = 
      case slay of
        "orcs" -> SlayOrcs
        "trolls" -> SlayTrolls
        "wolves" -> SlayWolves
        "spiders" -> SlaySpiders
        "undead" -> SlayUndead
        "dragons" -> SlayDragons
        "raukar" -> SlayRaukar

parseAbility :: Parser Ability
parseAbility = do 
  ability <- try (string "Finesse") <|>
             try (string "Power") <|>
             try (string "Subtlety") <|>
             try (string "Hardiness") <|>
             try (string "Assassination") <|>
             try (mlString "Two Weapon Fighting") <|>
             try (mlString "Rapid Attack") <|>
             try (mlString "Inner Light") <|>
             try (mlString "Critical Resistance") 
  return $ stringToAbility ability
  where
    stringToAbility :: String -> Ability
    stringToAbility str =
      case str of
        "Finesse" -> Finesse
        "Subtlety" -> Subtlety
        "Power" -> Power
        "Critical Resistance" -> CritRes
        "Hardiness" -> Hardiness
        "Two Weapon Fighting" -> TwoWeaponFighting
        "Rapid Attack" -> RapidAttack
        "Inner Light" -> InnerLight
        "Assassination" -> Assassination

--Parse abilities from the [Notes] section of a Sil char dump
parseAbilities = many $ (try $ ignoreUntil validAbility eof)
  where validAbility = between (char '(') (char ')') parseAbility


makeAttacks :: [(Int,Dice)] -> [Ability] -> [Equipment] -> [Attack]
makeAttacks attackTuples abilities equipment = 
  let eqSlots = inferSlots attackTuples abilities
      weapons = map (getWeapon equipment) eqSlots 
      getWeapon eq slot = head . filter (\x -> slot == eqSlot x) $ eq
  in map (makeAttack abilities) $ zip weapons attackTuples

makeAttack :: [Ability] -> (Equipment, (Int, Dice)) -> Attack
makeAttack abilities (weapon, (accuracy, damDice)) =
  let weaponWeight = fromJust $ eqWeight weapon 
      critThres = P.playerCritThres abilities weaponWeight
      brands = eqBrands weapon
      slays = eqSlays weapon
      sharpness = eqSharpness weapon
  in Attack { accuracy = accuracy,
              damage = damDice,
              brands = brands,
              slays = slays,
              sharpness = sharpness,
              critThreshold = critThres,
              --player attacks can always crit; this flag is for
              --monsters
              canCrit = True,
              alwaysHits = False}

--Collects all the elemental resistances and vulnerabilities from a
--list of Equipment and creates a map from Element to Int, where the
--number indicates the player's resistance level to that element.
--(E.g. two items providing cold resistance and one item providing
--cold vulnerability add up to (Cold, 1) in the map.)
resistancesFrom :: [Equipment] -> Map.Map Element Int
resistancesFrom eq = T.makeResistanceMap resList vulnList
  where resList = concat $ map eqResistances eq
        vulnList = concat $ map eqVulnerabilities eq

 



inferSlots :: [(Int, Dice)] -> [Ability] -> [EqSlot]
inferSlots attackTuples abilities
--If there's just one attack, it must be the main hand
  | numAttacks == 1 = [MainHand]
  | numAttacks == 2 && TwoWeaponFighting `elem` abilities
     = [MainHand, OffHand]
  | numAttacks == 2 && RapidAttack `elem` abilities
     = [MainHand, MainHand]
  | numAttacks == 3 = [MainHand, MainHand, OffHand]
    where numAttacks = length attackTuples
