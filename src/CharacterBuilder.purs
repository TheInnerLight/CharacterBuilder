module CharacterBuilder where
  
import Abilities
import Background
import Control.Alt
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested
import Prelude
import Races
import Skills

import Data.Array as A
import Data.Int (floor, round, toNumber)
import Data.Map as M
import Math (sqrt)
import SkillMap (SkillMap, costOfSkills, getSkillList, mapToArray, valueOfFreeSkills)
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

applyRacialStatBonus :: CharacterBuilder -> DerivedAbilities -> DerivedAbilities
applyRacialStatBonus cb pa =
  foldl (\_ race -> foldl disAdvFolder (foldl advFolder pa race.advantages) race.disadvantages) pa cb.race
  where 
  advFolder da (AbilityBonus ab v) = transformDerivedAbility ab (\x -> x + v) da
  advFolder da _ = da
  disAdvFolder da (AbilityPenalty ab v) = transformDerivedAbility ab (\x -> x - v) da
  disAdvFolder da _ = da

applySkillStatBonuses :: CharacterBuilder -> DerivedAbilities -> DerivedAbilities
applySkillStatBonuses cb da =
  foldl mapFolder da (mapToArray $ foldl skillFolder M.empty $ getSkillList cb.skills)
  where
  mapFolder acc (Tuple ability value) = transformDerivedAbility ability (abilityDiff value) acc
  skillFolder acc {skill : (Skill skill), subSkill : _, value: v} = M.alter (\o -> Just $ fromMaybe v $ map (\x -> x + v) o) skill.ability acc
  abilityDiff points oldAbility = floor $ sqrt(toNumber(oldAbility * oldAbility) + toNumber(points) / 2.5)

calculateDerivedAbilities :: CharacterBuilder -> DerivedAbilities
calculateDerivedAbilities cb = 
  applySkillStatBonuses cb <<< applyRacialStatBonus cb $ baseDerivedAbilities cb.abilities

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
