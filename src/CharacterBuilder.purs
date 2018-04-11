module CharacterBuilder where
  
import Abilities
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested
import Prelude
import Races
import Skills

import Background (Background)
import Data.Map as M
import SkillMap (mapToArray)
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
  Just (cb.abilityPoints - strCost - compCost - intCost - agCost)

costOfSkill :: Skill -> Int -> M.Map Skill (Tuple3 Int Int Int) -> Maybe Int
costOfSkill skill value modifiedBoundaries =
  uncurry3 (costWithBounds value) boundaries
  where
  costWithBounds value bound1 bound2 bound3 = 
    case value of
      v | v < bound1 -> Just v
      v | v < bound2 -> Just $ bound1 + (v - bound1) * 2
      v | v < bound3 -> Just $ bound1 + (bound2 - bound1) * 2 + (v - bound2) * 3
      v              -> Just $ bound1 + (bound2 - bound1) * 2 + (bound3 - bound2) * 3 + (v - bound3) * 4
  boundaries = fromMaybe (tuple3 3 6 9) (M.lookup skill modifiedBoundaries)

remainingSkillPoints :: CharacterBuilder -> Maybe Int
remainingSkillPoints cb = do
  cost <- totalCost $ derivedSkills cb
  costOfBackgroundSkills <- totalCost $ fromMaybe M.empty $ map (\bg -> bg.startingSkills) cb.background
  Just $ cb.skillPoints + costOfBackgroundSkills - cost
  where
  skillBoundaries = fromMaybe M.empty $ map (\r -> r.skillBoundaries) cb.race 
  totalCost skills = 
    let folder acc (Tuple skill value) = do 
         accuCost <- acc
         cost <- costOfSkill skill value skillBoundaries
         Just $ accuCost + cost in
    foldl (folder) (Just 0) $ map (uncurry3 \s _ v -> Tuple s v) $ SM.getSkillList skills

