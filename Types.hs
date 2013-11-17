module Types where

import Rdice
import qualified Data.Map as Map
import Data.List(group)

-- | Player abilities. These are the only ones that affect our current
-- combat model.
data Ability = Finesse | Subtlety | Power | CritRes | Hardiness 
  | TwoWeaponFighting | RapidAttack | InnerLight | Assassination
  | HeavyArmourUse | KeenSenses | PoisonResistance
  deriving (Show, Eq)

-- | A song (player ability). Currently only three songs are included
-- (the others wouldn't make much sense in the context of fsil anyway.)
data Song = SongSharp | SongTrees | SongStay
  deriving (Show, Eq)

-- | 'Singing' represents the player's singing status: either the player
-- is 'Quiet', singing a song or singing two songs with 'WovenThemes'. 
data Singing = Quiet | OneSong Song | WovenThemes (Song, Song)
  deriving (Show, Eq)


-- | Represents one of the equipment slots available to the player. 
data EqSlot = MainHand | BowSlot | FirstRing | SecondRing | AmuletSlot |
  LightSlot | ArmorSlot | CloakSlot | OffHand | HelmSlot | GloveSlot | 
  BootSlot | FirstQuiver | SecondQuiver
  deriving (Show, Eq, Ord)

type EquipmentMap = Map.Map EqSlot (Maybe Equippable)

data WeaponHandedness = OneHanded | HandAndAHalf | TwoHanded
  deriving (Show, Eq)


-- | An item that can be equipped by the player. The BaseEquip value encodes
-- properties that all equippable items have, and EquipType encodes the type
-- of item and properties that are specific to that type.
data Equippable = Equippable BaseEquip EquipType
  deriving (Show, Eq)

data BaseEquip = BaseEquip { 
    beqName :: String,
    beqSlot :: EqSlot,
    beqProtDice :: Maybe Dice, -- ^Protection granted by equipping this item
    beqResistances :: [Element],
    beqVulnerabilities :: [Element], 
    beqAbilities :: [Ability] --Abilities granted by the item
  }
  deriving (Show, Eq)

eqName (Equippable (BaseEquip {beqName = x}) _) = x
eqSlot (Equippable (BaseEquip {beqSlot = x}) _) = x
eqProtDice (Equippable (BaseEquip {beqProtDice = x}) _) = x
eqResistances (Equippable (BaseEquip {beqResistances = x}) _) = x
eqVulnerabilities (Equippable (BaseEquip {beqVulnerabilities = x}) _) = x
eqAbilities (Equippable (BaseEquip {beqAbilities = x}) _) = x

data EquipType = 
  Weapon {
    wpWeight :: Double, -- ^ Only weapons have weight listed in Sil char dumps.
    wpBrands :: [Element], 
    wpSlays :: [Slay], 
    wpSharpness :: Double,
    wpHandedness :: WeaponHandedness} 
  | Other 
  deriving (Show, Eq)
  -- Will replace Other with the following once simulation for equipment
  -- other than weapons is in place:
  -- Shield | Ring | Amulet | BodyArmor | Helm | Gloves | Boots 




-- | One of the elements in Sil: fire, cold, poison, dark or lightning.
-- Attacks can be branded with these, and players and monsters can be
-- vulnerable or resistant to them.
data Element = Fire | Cold | Poison | Dark | Lightning 
  deriving (Eq, Show, Ord)

--Values represent resistance levels; 0 means no resistance or vulnerability.
type ResistanceMap = Map.Map Element Int

noResistance :: ResistanceMap
noResistance = Map.fromList [(Fire,0), (Cold,0), (Poison,0), (Lightning,0), 
  (Dark,0)]

-- | Takes two 'Element' lists: one that represents resistances and one 
-- that represents their vulnerabilities. If an element occurs N times in
-- the resistance list, this counts as N levels of resistance; likewise
-- for vulnerabilities.
makeResistanceMap :: [Element] -> [Element] -> ResistanceMap
makeResistanceMap resList vulnList = 
  let groupedRes = group resList
      groupedVuln = group vulnList
      makeResTuple elGroup = (head elGroup, length elGroup)
      resTuples = map makeResTuple groupedRes
      vulnTuples = map makeResTuple groupedVuln
      baseRes = noResistance
      insertResTupleWith func (elem', level) = Map.insertWith func elem' level
      insertResTuplesWith f tuples resMap 
        = foldr (insertResTupleWith f) resMap tuples
      subtract = \x y -> y - x
  in insertResTuplesWith subtract vulnTuples $ insertResTuplesWith (+) resTuples $ baseRes
 


-- | One of Sil's slaying modifiers, e.g. 'slay orcs'. Weapons can have these.
data Slay = SlayOrcs | SlayRaukar | SlayDragons | SlaySpiders | SlayWolves 
  | SlayUndead | SlayTrolls
  deriving (Eq, Show)

-- | A single attack of a player or monster. Contains everything we need to
-- know about the attacker to simulate attacks on the defender: accuracy, 
-- damage dice, brands (if any), slays (if any), sharpness (if any) and
-- the critical threshold.
data Attack = Attack { accuracy :: Int,
                       damage :: Dice,
                       brands :: [Element],
                       slays :: [Slay],
                       --sharpness is just the modifier by which the target's
                       --protection roll is multiplied
                       sharpness :: Double,
                       critThreshold :: Double,
                       canCrit :: Bool, -- ^Some attacks can't crit
                       alwaysHits :: Bool} -- ^Some attacks always hit
  deriving (Show)
