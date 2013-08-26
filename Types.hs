module Types where

import Dice

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
    eqSharpness :: Sharpness 
  }
  deriving (Show, Eq)

data Element = Fire | Cold | Poison | Dark | Lightning 
  deriving (Eq, Show, Ord)

data Sharpness = NotSharp | Sharp | VerySharp
  deriving (Eq, Show)


data Slay = SlayOrcs | SlayRaukar | SlayDragons | SlaySpiders | SlayWolves 
  | SlayUndead | SlayTrolls
  deriving (Eq, Show)

data Attack = Attack { accuracy :: Int,
                       damage :: Dice,
                       brands :: [Element],
                       slays :: [Slay],
                       sharpness :: Sharpness,
                       critThreshold :: Double }
  deriving (Show)
