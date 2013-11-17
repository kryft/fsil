-- | A module for scraping Sil 1.1.1 character dump files.
module CharDumpParser (readCharDump, charDumpFile) where

import Text.Parsec
import Data.Char(isSpace)
import Text.Parsec.String (Parser, parseFromFile)
import qualified Player as P
import Data.List (concat, group)
import Data.Maybe (catMaybes, fromJust, isJust)
import qualified Data.Map as Map
import Rdice (Dice(ZeroDie), d, nSides, nDice)
import GeneralParse
import Types as T
import Control.Monad (when)


-- | Takes the name of a char dump file and tries to construct a Player
-- from the dump. Will call error if the file doesn't exist or if the 
-- parser fails (shouldn't happen unless there's a bug).
readCharDump :: String -> IO P.Player
readCharDump filename = 
  do
    maybePlayer <- parseFromFile charDumpFile filename
    case maybePlayer of
      Right player -> return player
      Left parseError -> error $ show parseError

-- | Parsec Parser for Sil 1.1.1 dump files.
charDumpFile :: Parser P.Player
charDumpFile = 
--The char dump format is not designed to be parsed, and much of the 
--information is irrelevant for fsil, so there's a lot of skipping
--ahead to the next relevant bit using skipTill. Occasionally lookAhead
--is used to make multiple passes over some part of the file to parse
--several different things.
  do
    skipTill "Name"
    name <- parseName
    skipTill "Melee"
    attackTuples <- lookAhead $ parseAttackTuples 
    listedProtRange <- lookAhead $ skipTill "Armor" >> parseProtRange
    evasion <- getStat "Evasion" 
    stealth <- getStat "Stealth"
    will <- getStat "Will"
    lightRadius <- getStat "Light Radius"
    song <- getStat "Song"
    skipTill "[Equipment]"
    equipment <- parseEquipment
    skipTill "[Notes]"
    abilities <- parseAbilities
    let items = catMaybes $ Map.elems equipment
        protDice = 
          (filter (/= ZeroDie)) . catMaybes $ map eqProtDice items
        eqProtMax = sum $ map (\x -> nDice x * nSides x) protDice
        heavyArmourUseProtBonus = 
          inferHeavyArmourUseBonus listedProtRange eqProtMax abilities will 
        eqAbilities' = concat $ map eqAbilities items
        allAbilities = abilities ++ eqAbilities'
        eqRes = resistancesFrom equipment
        resists = if (T.PoisonResistance `elem` abilities)
          then Map.insertWith (+) T.Poison 1 eqRes
          else eqRes
        attacks = makeAttacks attackTuples allAbilities equipment
    return $ P.Player 
      { P.name = name,
        P.attacks = attacks,
        P.lightRadius = lightRadius,
        P.evasion = evasion,
        P.stealth = stealth,
        P.resistances = resists,
        P.will = will,
        P.song = song,
        P.activeSongs = Quiet,
        P.equipment = equipment,
        P.protDice = protDice ++ heavyArmourUseProtBonus,
        P.abilities = abilities,
        P.onLitSquare = False
      }

--A necessary hack: armor weight isn't listed in char dumps, so
--in order to figure out how big the protection bonus from the Heavy Armour
--Use ability is, we look at the listed max protection roll (e.g. "18" in
--"Armor [+13,6-18]") and subtract the contributions of all other sources of 
--protection (whose size can be inferred from the char dump), that is,
--equipment, Hardiness and Song of Staying.
--FIXME: actually include Song of Staying :P
inferHeavyArmourUseBonus :: 
  (Int,Int) -> Int -> [T.Ability] -> Int -> [Dice]
inferHeavyArmourUseBonus listedProtRange eqProtMax abilities will =
  if not (T.HeavyArmourUse `elem` abilities)
  then []
  else
    let hardinessBonus = if (T.Hardiness `elem` abilities)
                           then will `quot` 6
                           else 0
        listedProtMax = snd listedProtRange
        heavyArmourUseSides = listedProtMax - hardinessBonus - eqProtMax
    in [1 `d` heavyArmourUseSides]


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

--Parses all the player's melee attacks of the form (+X, YdZ)
parseAttackTuples :: Parser [(Int,Dice)]
parseAttackTuples = try $ do
  spaces 
  (try parseAttackTuple) `endBy1` junk
  where 
    junk = anyChar `manyTill` lookAhead (string "(" <|> string "Bows")

--Parses the listed protection range, e.g. (6,18) in "Armor [+13, 6-18]"
parseProtRange :: Parser (Int,Int)
parseProtRange = try $ do
  skipTill ","
  minProt <- parseInt
  char '-'
  maxProt <- parseInt
  return $ (minProt,maxProt)


--Parse the "Equipment" section of the char dump file
parseEquipment :: Parser EquipmentMap
parseEquipment = do
  anyChar `manyTill` lookAhead slotID
  eqSlots <- eqSlotEntry `sepEndBy` endItemDesc
  return $ foldr (uncurry Map.insert) Map.empty eqSlots 


--Description of a single equipment slot; can have an item or be empty.
eqSlotEntry :: Parser (EqSlot, Maybe Equippable)
eqSlotEntry = do
  slotType <- eqSlotType
  slotContent <- try emptySlot <|> fmap Just (item slotType)
  return (slotType, slotContent)
 
--Beginning of an equipment slot description, e.g. "a)"
slotID :: Parser String
slotID = try $ oneOf "abcdefghijklmn" >>= \id -> char ')' >> return [id, ')']

eqSlotType :: Parser EqSlot
eqSlotType = fmap idToSlot slotID
  where
  idToSlot slotStr = case slotStr of
    "a)" -> MainHand
    "b)" -> BowSlot
    "c)" -> FirstRing
    "d)" -> SecondRing
    "e)" -> AmuletSlot
    "f)" -> LightSlot
    "g)" -> ArmorSlot
    "h)" -> CloakSlot
    "i)" -> OffHand
    "j)" -> HelmSlot
    "k)" -> GloveSlot
    "l)" -> BootSlot
    "m)" -> FirstQuiver
    "n)" -> SecondQuiver

emptySlot :: Parser (Maybe Equippable)
emptySlot = spaces >> (string "(nothing)") >> spaces >> eol >> return Nothing

item :: T.EqSlot -> Parser Equippable
item slotType = do
  name <- do
    spaces  
    (anyChar `manyTill` (lookAhead $ (oneOf "([<") <|> (eol >> return ' ')))

  optional $ try $ between (char '(') (char ')') 
    (string "Defender" <|> string "Vampiric" <|> string "Poisoned")
  many spaceBar
  --Skip possible attack tuple
  optional $ try $ parseAttackTuple
  many spaceBar
  --Just about any equipped item may grant protection
  maybeProtDice <- option Nothing (fmap (Just . snd) parseDefenseTuple) 
  --Skip possible stat bonus 
  many spaceBar
  optional $ between (char '<') (char '>') (many $ noneOf ">")
  many spaceBar
  --Skip possible inscription
  optional $ between (char '{') (char '}') (many $ noneOf "}")
  --Only weapons have weight listed here (and we don't care about
  --weight for other items anyway)
  many spaceBar
  maybeWeight <- option Nothing $ fmap Just parseWeight
  let isMeleeWeapon = isJust maybeWeight && slotType /= BowSlot
  resistances <- lookAhead $ parseResistances
  vulnerabilities <- lookAhead $ parseVulnerabilities
  abilities <- lookAhead $ parseItemAbilities
  let baseEquip = BaseEquip { beqName = stripSpacesAtEnd $ name,
                              beqSlot = slotType,
                              beqProtDice = maybeProtDice,
                              beqResistances = resistances,
                              beqAbilities = abilities,
                              beqVulnerabilities = vulnerabilities }
  --Weapon-specific stuff:
  brands <- lookAhead parseBrands 
  slays <- lookAhead parseSlays 
  sharpness <- lookAhead parseSharpness 
  handedness <- lookAhead parseHandedness

  anyChar `manyTill` (lookAhead endItemDesc)
  let equipSpecific  
        | isMeleeWeapon = Weapon {wpBrands = brands,
                              wpSlays = slays,
                              wpSharpness = sharpness,
                              wpHandedness = handedness,
                              wpWeight = fromJust maybeWeight}
        | otherwise = Other

  return $ Equippable baseEquip equipSpecific


--Parse the weight of an item (char dump only lists this for weapons)
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

parseSharpness = option 1.0 $ itemDescScraper sharpSentence 
  where sharpSentence = try (mlString "cuts easily through armour" >> (return 0.5)) <|> (mlString "cuts very easily through armour" >> (return 0.0))

parseHandedness = option OneHanded $ itemDescScraper handednessSentence
  where handednessSentence = try (mlString "It does extra damage when wielded with both hands." >> (return HandAndAHalf)) <|> (mlString "It requires both hands to wield it properly." >> (return TwoHanded))


--mlString parses a string, but allows it to span multiple lines.
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
             try (string "venom") <|>
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
        "venom" -> Poison
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
             try (mlString "Critical Resistance") <|>
             try (mlString "Keen Senses") <|>
             try (mlString "Heavy Armour Use") <|>
             try (mlString "Poison Resistance") 
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
        "Heavy Armour Use" -> HeavyArmourUse
        "Keen Senses" -> KeenSenses
        "Poison Resistance" -> PoisonResistance

--Parse abilities from the [Notes] section of a Sil char dump
parseAbilities = many $ (try $ ignoreUntil validAbility eof)
  where validAbility = between (char '(') (char ')') parseAbility


--Construct Attack values for the player based on the attacks listed
--after "Melee" in the dump.
makeAttacks :: [(Int,Dice)] -> [Ability] -> EquipmentMap -> [Attack]
makeAttacks attackTuples abilities equipment = 
  let eqSlots = inferSlots attackTuples abilities
      weapons = map (fromJust . ((Map.!) equipment)) eqSlots 
  in map (makeAttack abilities) $ zip weapons attackTuples

--Constructs a single Attack value, taking into account relevant abilities 
--and modifiers from the weapon
makeAttack :: [Ability] -> (Equippable, (Int, Dice)) -> Attack
makeAttack abilities (Equippable _ (Weapon {wpBrands = brands, 
                                            wpSlays = slays, 
                                            wpSharpness = sharpness,
                                            wpWeight = weaponWeight})
                                   , (accuracy, damDice)) =
  let critThres = P.playerBaseCritThres + weaponWeight
  in Attack { accuracy = accuracy,
              damage = damDice,
              brands = brands,
              slays = slays,
              sharpness = sharpness,
              critThreshold = critThres,
              --player attacks can always crit
              canCrit = True,
              alwaysHits = False}

--Collects all the elemental resistances and vulnerabilities from a
--list of Equipment and creates a map from Element to Int, where the
--number indicates the player's resistance level to that element.
--(E.g. two items providing cold resistance and one item providing
--cold vulnerability add up to (Cold, 1) in the map.)
resistancesFrom :: EquipmentMap -> Map.Map Element Int
resistancesFrom eqMap = T.makeResistanceMap resList vulnList
  where eqList = catMaybes $ Map.elems eqMap
        resList = concat $ map eqResistances eqList
        vulnList = concat $ map eqVulnerabilities eqList

 


--Infers which attack tuple listed after "Melee" in the char dump
--corresponds to which weapon. For example, if there are two attack
--tuples, it could either be two attacks with the main weapon or
--one attack with the main weapon and one attack with the off-hand.
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
