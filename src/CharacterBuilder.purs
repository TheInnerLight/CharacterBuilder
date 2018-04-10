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
import DOM.HTML.HTMLElement (offsetHeight)
import Data.Array as A
import Data.Map as M
import Data.Set as S
import React.DOM.Props (x)
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
  case value of
    v | v < get1 boundaries -> Just v
    v | v < get2 boundaries -> Just (get1 boundaries + (v - get1 boundaries) * 2 )
    v | v < get3 boundaries -> Just (get1 boundaries + (get2 boundaries - get1 boundaries) * 2 + (v - get2 boundaries) * 3  )
    v -> Just (get1 boundaries + (get2 boundaries - get1 boundaries) * 2 + (get3 boundaries - get2 boundaries) * 3 + (v - get3 boundaries) * 4 )
  where
  boundaries = fromMaybe (tuple3 3 6 9) (M.lookup skill modifiedBoundaries)

remainingSkillPoints :: CharacterBuilder -> Maybe Int
remainingSkillPoints cb = do
  cost <- foldl (folder) (Just 0) skillList
  Just $ cb.skillPoints - cost
  where
  folder acc (Tuple skill value) = do 
    accuCost <- acc
    cost <- costOfSkill skill value skillBoundaries
    Just $ accuCost + cost
  --folder acc (Tuple skill value) = costOfSkill skill value skillBoundaries
  skillBoundaries = fromMaybe M.empty $ map (\r -> r.skillBoundaries) cb.race 
  skillList = foldl (\acc (Tuple k v) -> append (map (Tuple k) (M.values v)) acc) Nil $ mapToArray cb.skills