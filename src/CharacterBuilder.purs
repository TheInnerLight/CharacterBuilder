module CharacterBuilder where
  
import Abilities
import Data.List
import Data.Maybe
import Data.Map as Map
import Prelude
import Races
import Skills
import Background (Background)

type CharacterBuilder = 
  { abilityPoints :: Int
  , skillPoints :: Int
  , abilities :: PrimaryAbilities
  , skills :: Map.Map Skill Int
  , background :: Maybe Background
  , race :: Maybe Race
  }

remainingAbilityPoints :: CharacterBuilder -> Maybe Int
remainingAbilityPoints cb = do
  strCost <- baseCostOfAbility cb.abilities.strength
  compCost <- baseCostOfAbility cb.abilities.comprehension
  intCost <- baseCostOfAbility cb.abilities.intuition 
  agCost <- baseCostOfAbility cb.abilities.agility
  Just (cb.abilityPoints - strCost - compCost - intCost - agCost)