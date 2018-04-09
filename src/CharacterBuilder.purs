module CharacterBuilder where
  
import Abilities
import Data.List
import Data.Maybe
import Prelude
import Races
import Skills

import Background (Background)
import Data.Map as Map

type CharacterBuilder = 
  { abilityPoints :: Int
  , skillPoints :: Int
  , abilities :: BaseAbilities
  , skills :: Map.Map Skill Int
  , background :: Maybe Background
  , race :: Maybe Race
  }

applyRacialStatBonus :: Race -> DerivedAbilities -> DerivedAbilities
applyRacialStatBonus race pa =
  foldl folder pa race.advantages
  where 
  folder acc (AbilityBonus Strength v) = acc {strength = acc.strength + v}
  folder acc (AbilityBonus Agility v) = acc {agility = acc.agility + v}
  folder acc (AbilityBonus Intuition v) = acc {intuition = acc.intuition + v}
  folder acc (AbilityBonus Comprehension v) = acc {comprehension = acc.comprehension + v}
  folder acc (AbilityBonus Health v) = acc {health = acc.health + v}
  folder acc (AbilityBonus Resolve v) = acc {resolve = acc.resolve + v}
  folder acc _ = acc

calculateDerivedAbilities :: CharacterBuilder -> DerivedAbilities
calculateDerivedAbilities cb = 
    foldl (\acc race -> applyRacialStatBonus race acc) baseDerivedAbilities cb.race
    where
    baseDerivedAbilities = 
        { strength : cb.abilities.strength
        , agility : cb.abilities.agility
        , intuition : cb.abilities.intuition
        , comprehension : cb.abilities.comprehension
        , health : (cb.abilities.strength + cb.abilities.agility)/2
        , resolve : (cb.abilities.intuition + cb.abilities.comprehension)/2
        }

abilityScore :: Ability -> CharacterBuilder -> Int
abilityScore Strength cb = cb.abilities.strength
abilityScore Agility cb = cb.abilities.agility
abilityScore Comprehension cb = cb.abilities.comprehension
abilityScore Intuition cb = cb.abilities.intuition
abilityScore _ cb = 0

remainingAbilityPoints :: CharacterBuilder -> Maybe Int
remainingAbilityPoints cb = do
  strCost <- baseCostOfAbility cb.abilities.strength
  compCost <- baseCostOfAbility cb.abilities.comprehension
  intCost <- baseCostOfAbility cb.abilities.intuition 
  agCost <- baseCostOfAbility cb.abilities.agility
  Just (cb.abilityPoints - strCost - compCost - intCost - agCost)