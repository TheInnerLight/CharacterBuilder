module SkillMap where
import Data.Maybe
import Data.Unfoldable
import Prelude
import Skills

import Data.Array as A
import Data.Map as M
import Data.Tuple (Tuple(..))

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