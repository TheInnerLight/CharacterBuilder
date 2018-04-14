module CharacterBuilder where
  
import Abilities
import Background
import Control.Alt
import Data.Array as A
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested
import Prelude
import Races
import Skills

import Data.Map as M
import SkillMap (costOfSkills, mapToArray, valueOfFreeSkills)
import SkillMap as SM

type CharacterBuilder = 
  { abilityPoints :: Int
  , skillPoints :: Int
  , abilities :: BaseAbilities
  , skills :: SM.SkillMap
  , background :: Maybe Background
  , race :: Maybe Race
  }

derivedSkills :: CharacterBuilder -> SM.SkillMap
derivedSkills cb =
  SM.merge cb.skills $ backgroundSkillSet cb.background
  where 
  backgroundSkillSet (Nothing) = SM.empty
  backgroundSkillSet (Just background) = background.startingSkills

transformAbility :: Ability -> (Int -> Int) -> CharacterBuilder -> CharacterBuilder
transformAbility Strength f cb =
  cb { abilities { strength = f cb.abilities.strength} }
transformAbility Agility f cb =
  cb { abilities { agility = f cb.abilities.agility} }
transformAbility Comprehension f cb =
  cb { abilities { comprehension = f cb.abilities.comprehension} }
transformAbility Intuition f cb =
  cb { abilities { intuition = f cb.abilities.intuition} }
transformAbility _ _ cb = cb

transformSkill :: Skill -> Maybe String -> (Int -> Int) -> CharacterBuilder -> CharacterBuilder
transformSkill skill maybeSubskill f cb =
  cb { skills = SM.update skill maybeSubskill (f) cb.skills }

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

remainingAbilityPoints :: CharacterBuilder -> Maybe Int
remainingAbilityPoints cb = do
  strCost <- baseCostOfAbility cb.abilities.strength
  compCost <- baseCostOfAbility cb.abilities.comprehension
  intCost <- baseCostOfAbility cb.abilities.intuition 
  agCost <- baseCostOfAbility cb.abilities.agility
  Just (cb.abilityPoints + freePoints - strCost - compCost - intCost - agCost)
  where
  folder acc (AnyAbilityBonus bonus) = acc + bonus
  folder acc _ = acc
  freePoints = fromMaybe 0 $ map (\r -> foldl (folder) 0 r.advantages) cb.race

remainingSkillPoints :: CharacterBuilder -> Int
remainingSkillPoints cb =
  let variableFreeSkillCost = valueOfFreeSkills boundaries freeSkills cb.skills in 
  let backgroundSkillCost = costOfSkills boundaries $ backgroundSkills in
  let combinedCost = costOfSkills boundaries $ combinedSkills in
  cb.skillPoints + variableFreeSkillCost + backgroundSkillCost - combinedCost
  where
  combinedSkills = derivedSkills cb
  backgroundSkills = fromMaybe M.empty $ map (\bg -> bg.startingSkills) cb.background
  freeSkills = fromMaybe [] $ map (\bg -> bg.freeSkillBonuses) cb.background
  boundaries = fromMaybe M.empty $ map (\r -> r.skillBoundaries) cb.race
