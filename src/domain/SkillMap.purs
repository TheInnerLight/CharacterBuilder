module SkillMap where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (foldl)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested (Tuple3, tuple3, uncurry3)
import Skills (FreeSkillBonus(..), Skill, SkillBoundaries, SkillValue, costOfBuyingSkill)

type SkillMap = M.Map Skill (M.Map (Maybe String) Int)

empty :: SkillMap
empty = M.empty

mapToArray :: forall k v. M.Map k v -> Array (Tuple k v)
mapToArray m = M.toUnfoldable m

singleSkill :: Skill -> Int -> Tuple Skill (M.Map (Maybe String) Int)
singleSkill skill value = Tuple skill (M.fromFoldable [Tuple Nothing value])

relatedSkills :: Skill -> M.Map String Int -> Tuple Skill (M.Map (Maybe String) Int)
relatedSkills skill value = Tuple skill (M.fromFoldable $ map (\(Tuple k v) -> Tuple (Just k) v) $ mapToArray value)

merge :: SkillMap -> SkillMap -> SkillMap
merge sm1 sm2 = M.unionWith (M.unionWith (\s1 s2 -> s1 + s2)) sm1 sm2

update :: Skill -> Maybe String -> (Int -> Int) -> SkillMap -> SkillMap 
update skill maybeString f skillMap = M.update (\m -> Just $ updateInternal m) skill skillMap
  where
  updateInternal subSkillMap = M.alter (updater $ f) maybeString subSkillMap
  updater f (Just x) = Just $ f x
  updater f (Nothing) = Just $ f 0

removeSkill :: Skill -> Maybe String -> SkillMap -> SkillMap 
removeSkill skill maybeString skillMap = M.update (\m -> Just $ updateInternal m) skill skillMap
  where
  updateInternal subSkillMap = M.alter (const Nothing) maybeString subSkillMap

getSkillValue :: Skill -> Maybe String -> SkillMap -> Maybe Int
getSkillValue skill maybeString skillMap = do 
  subSkillMap <- M.lookup skill skillMap
  value <- M.lookup maybeString subSkillMap
  Just value

getSkillList :: SkillMap -> L.List SkillValue
getSkillList skillMap = foldl folder L.Nil $ mapToArray skillMap
  where
  folder acc (Tuple k v) = append (map (\(Tuple s v) -> {skill : k, subSkill : s, value: v}) (L.fromFoldable $ mapToArray v)) acc

costOfSkills :: SkillBoundaries -> SkillMap -> Int
costOfSkills boundaries skills = foldl (\acc {skill : skill, subSkill : _, value : value} -> acc + costOfBuyingSkill boundaries skill value) 0 $ getSkillList skills

valueOfFreeSkills :: SkillBoundaries -> Array FreeSkillBonus -> SkillMap -> Int
valueOfFreeSkills boundaries freeSkills skills =
  snd $ foldl applyVariableFreeSkill (Tuple skills 0) freeSkills
  where 
  skillOfAnySubtypeWithMinimum :: Int -> Skill -> SkillMap -> Maybe {key :: Maybe String, value :: Int}
  skillOfAnySubtypeWithMinimum minimum skill skillMap = do
    subSkillMap <- M.lookup skill skillMap
    subSkillsAtLeast <- Just $ M.filter (\x -> x >= minimum) subSkillMap
    keyValue <- M.findMin subSkillsAtLeast
    Just keyValue
  applyVariableFreeSkill (Tuple m saving) (SpecificSkill skill v) = fromMaybe (Tuple m saving) $ maybeSavingFolder skill m saving v
  applyVariableFreeSkill (Tuple m saving) (OneOfTwoSkills skill1 skill2 v) = fromMaybe (Tuple m saving) $ maybeSavingFolder skill1 m saving v <|> maybeSavingFolder skill2 m saving v
  applyVariableFreeSkill (Tuple m saving) (OneOfThreeSkills skill1 skill2 skill3 v) = fromMaybe (Tuple m saving) $ maybeSavingFolder skill1 m saving v <|> maybeSavingFolder skill2 m saving v <|> maybeSavingFolder skill3 m saving v
  maybeSavingFolder skill m saving freeSkills = 
    let freeSkillPoints = costOfBuyingSkill boundaries skill freeSkills in
    case skillOfAnySubtypeWithMinimum freeSkills skill m of
    Just {key : key, value : value} | value >= freeSkills -> Just $ Tuple (M.update (\sm -> Just $ M.update (\_ -> Nothing) key sm) skill m) (saving + freeSkillPoints)
    _                                                     -> Nothing
         


