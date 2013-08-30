module Types where

import Dice
import qualified Data.Map as Map
import Data.List(group)

--These are the only abilities that affect our current combat model;
--if you want to simulate something like Dodging or Concentration,
--you can modify the player's melee/evasion scores with 
--modifyEvasionWith and modifyAccuracyWith
data Ability = Finesse | Subtlety | Power | CritRes | Hardiness 
  | TwoWeaponFighting | RapidAttack | InnerLight | Assassination
  deriving (Show, Eq)

data Song = SongSharp | SongTrees | SongStay
  deriving (Show, Eq)

--No song, one song or (Major theme, Minor theme)
data Singing = Quiet | OneSong Song | WovenThemes (Song, Song)
  deriving (Show, Eq)


--Currently the only information we need about the equipment slot of an item
--is whether the item is the main weapon or in the off-hand slot. 
data EqSlot = MainHand | OffHand | OtherSlot
  deriving (Show, Eq)

--Currently we only keep track of information that can only be obtained by
--looking at the item's description in the character sheet
data Equipment = Equipment 
  { 
    eqSlot :: EqSlot, 
    eqName :: String,
    eqWeight :: Maybe Double, --Nothing if the item is not a weapon
    eqIsMeleeWeapon :: Bool,
    eqProtDice :: Maybe Dice, --Protection granted by equipping this item
    eqResistances :: [Element],
    eqVulnerabilities :: [Element],
    eqAbilities :: [Ability], --Abilities granted by the item
    --The last three are only relevant for weapons
    eqBrands :: [Element], 
    eqSlays :: [Slay], 
    eqSharpness :: Double 
  }
  deriving (Show, Eq)

data Element = Fire | Cold | Poison | Dark | Lightning 
  deriving (Eq, Show, Ord)

noResistance = Map.fromList [(Fire,0), (Cold,0), (Poison,0), (Lightning,0), 
  (Dark,0)]

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
  in insertResTuplesWith (-) vulnTuples $ insertResTuplesWith (+) resTuples $ baseRes
 


data Slay = SlayOrcs | SlayRaukar | SlayDragons | SlaySpiders | SlayWolves 
  | SlayUndead | SlayTrolls
  deriving (Eq, Show)

data Attack = Attack { accuracy :: Int,
                       damage :: Dice,
                       brands :: [Element],
                       slays :: [Slay],
                       --sharpness is just the modifier by which the target's
                       --protection roll is multiplied
                       sharpness :: Double,
                       critThreshold :: Double,
                       canCrit :: Bool,
                       alwaysHits :: Bool}
  deriving (Show)
