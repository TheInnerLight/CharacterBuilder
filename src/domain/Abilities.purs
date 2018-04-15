module Abilities where
  
import Prelude
import Data.Maybe

data Ability 
  = Strength 
  | Comprehension 
  | Agility 
  | Intuition
  | Health
  | Resolve

instance showAbility :: Show Ability where
  show Strength      = "Strength"
  show Comprehension = "Comprehension"
  show Agility       = "Agility"
  show Intuition     = "Intuition"
  show Health        = "Health"
  show Resolve       = "Resolve"

derive instance eqAbility :: Eq Ability
derive instance ordAbility :: Ord Ability

type BaseAbilities = 
  { strength :: Int
  , comprehension :: Int
  , agility :: Int
  , intuition :: Int 
  }

type DerivedAbilities = 
  { strength :: Int
  , comprehension :: Int
  , agility :: Int
  , intuition :: Int 
  , health :: Int
  , resolve :: Int
  }

baseCostOfAbility :: Int -> Maybe Int
baseCostOfAbility 0 = Just 0
baseCostOfAbility 1 = Just 0
baseCostOfAbility 2 = Just 1
baseCostOfAbility 3 = Just 2
baseCostOfAbility 4 = Just 3
baseCostOfAbility 5 = Just 5
baseCostOfAbility _ = Nothing

abilityScore :: Ability -> BaseAbilities -> Int
abilityScore Strength abilities = abilities.strength
abilityScore Agility abilities = abilities.agility
abilityScore Comprehension abilities = abilities.comprehension
abilityScore Intuition abilities = abilities.intuition
abilityScore _ abilities = 0

baseDerivedAbilities :: BaseAbilities -> DerivedAbilities
baseDerivedAbilities ba = 
  { strength : ba.strength
  , agility : ba.agility
  , intuition : ba.intuition
  , comprehension : ba.comprehension
  , health : (ba.strength + ba.agility)/2
  , resolve : (ba.intuition + ba.comprehension)/2
  }

transformDerivedAbility :: Ability -> (Int -> Int) -> DerivedAbilities -> DerivedAbilities
transformDerivedAbility Strength f da = da { strength = f da.strength }
transformDerivedAbility Agility f da = da { agility = f da.agility }
transformDerivedAbility Intuition f da = da { intuition = f da.intuition }
transformDerivedAbility Comprehension f da = da { comprehension = f da.comprehension }
transformDerivedAbility Health f da = da { health = f da.health }
transformDerivedAbility Resolve f da = da { resolve = f da.resolve }