module Types where

import Rdice
import qualified Data.Map as Map
import Data.List(group)

-- | Player abilities. These are the only ones that affect our current
-- combat model.
data Ability = Finesse | Subtlety | Power | CritRes | Hardiness 
  | TwoWeaponFighting | RapidAttack | InnerLight | Assassination
  | HeavyArmourUse | KeenSenses
  deriving (Show, Eq)

-- | A song (player ability). Currently only three songs are included
-- (the others wouldn't make much sense in the context of fsil anyway.)
data Song = SongSharp | SongTrees | SongStay
  deriving (Show, Eq)

-- | 'Singing' represents the player's singing status: either the player
-- is 'Quiet', singing 'OneSong' or singing two songs with 'WovenThemes'. 
data Singing = Quiet | OneSong Song | WovenThemes (Song, Song)
  deriving (Show, Eq)


-- | Represents whether an equipped item is in the main weapon slot, the 
-- off-hand slot, or another slot.
data EqSlot = MainHand | OffHand | OtherSlot
  deriving (Show, Eq)

--Currently we only keep track of information that can only be obtained by
--looking at the item's description in the character sheet

-- | Represents an item equipped by the player. 
data Equipment = Equipment 
  { 
    eqSlot :: EqSlot, 
    eqName :: String,
    eqWeight :: Maybe Double, -- ^Nothing if the item is not a weapon
    eqIsMeleeWeapon :: Bool,
    eqProtDice :: Maybe Dice, -- ^Protection granted by equipping this item
    eqResistances :: [Element],
    eqVulnerabilities :: [Element], 
    eqAbilities :: [Ability], --Abilities granted by the item
    --The last three are only relevant for weapons
    eqBrands :: [Element], 
    eqSlays :: [Slay], 
    eqSharpness :: Double 
  }
  deriving (Show, Eq)

-- | One of the elements in Sil: fire, cold, poison, dark or lightning.
-- Attacks can be branded with these, and players and monsters can be
-- vulnerable or resistant to them.
data Element = Fire | Cold | Poison | Dark | Lightning 
  deriving (Eq, Show, Ord)

noResistance = Map.fromList [(Fire,0), (Cold,0), (Poison,0), (Lightning,0), 
  (Dark,0)]

-- | Takes two 'Element' lists: one that represents resistances and one 
-- that represents their vulnerabilities. If an element occurs N times in
-- the resistance list, this counts as N levels of resistance; likewise
-- for vulnerabilities.
--
-- Returns a map with 'Element' keys and 'Int' values that can be queried
-- for resistance level (0 represents no resistance or vulnerability).
makeResistanceMap :: [Element] -> [Element] -> Map.Map Element Int
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
