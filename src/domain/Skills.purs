module Skills where

import Abilities
import Data.Maybe
import Data.Newtype
import Data.Map as M
import Data.Tuple.Nested
import Prelude

data SkillType 
  = FieldSpecific
  | SingleValue

newtype Skill = Skill
  { name :: String
  , skillType :: SkillType
  , ability :: Ability
  }

type SkillBoundary = {cost2 :: Int, cost3 :: Int, cost4 :: Int}
type SkillBoundaries = M.Map Skill SkillBoundary

data FreeSkillBonus
  = SpecificSkill Skill Int
  | OneOfTwoSkills Skill Skill Int
  | OneOfThreeSkills Skill Skill Skill Int

instance freeSkillBonusShow :: Show FreeSkillBonus where
  show (SpecificSkill (Skill skill) value) = show value <> " free ranks in one " <> skill.name <> " field skill"
  show (OneOfTwoSkills (Skill skill1) (Skill skill2) value) = show value <> " free ranks in one " <> skill1.name <> " or " <> skill2.name <> " field skill"
  show (OneOfThreeSkills (Skill skill1) (Skill skill2) (Skill skill3) value) = show value <> " free ranks in one " <> skill1.name <> " or " <> skill2.name <> " or " <> skill3.name <> " field skill"

derive instance newtypeSkill :: Newtype Skill _
derive instance eqSkill :: Eq Skill
derive instance ordSkill :: Ord Skill
derive instance eqSkillType :: Eq SkillType
derive instance ordSkillType :: Ord SkillType

defaultSkillBoundary :: SkillBoundary
defaultSkillBoundary = {cost2 : 3, cost3: 6, cost4: 9}

costOfBuyingSkill :: SkillBoundaries -> Skill -> Int -> Int
costOfBuyingSkill boundaries skill value =
  let boundary = fromMaybe defaultSkillBoundary $ M.lookup skill $ boundaries in 
  costWithBounds value boundary
  where
  costWithBounds value {cost2 : bound1, cost3 : bound2, cost4 : bound3} = 
    case value of
      v | v <= bound1 -> v
      v | v <= bound2 -> bound1 + (v - bound1) * 2
      v | v <= bound3 -> bound1 + (bound2 - bound1) * 2 + (v - bound2) * 3
      v               -> bound1 + (bound2 - bound1) * 2 + (bound3 - bound2) * 3 + (v - bound3) * 4

academics :: Skill
academics = Skill { name : "Academics", skillType : FieldSpecific, ability : Comprehension }

acrobatics :: Skill
acrobatics = Skill { name : "Acrobatics", skillType : SingleValue, ability : Agility }

art :: Skill
art = Skill { name : "Art", skillType : FieldSpecific, ability : Intuition }

athletics :: Skill
athletics = Skill { name : "Athletics", skillType : SingleValue, ability : Strength }

deception :: Skill
deception = Skill { name : "Deception", skillType : SingleValue, ability : Intuition }

demolitions :: Skill
demolitions = Skill { name : "Demolitions", skillType : SingleValue, ability : Comprehension }

disguise :: Skill
disguise = Skill { name : "Disguise", skillType : SingleValue, ability : Intuition }

engineering :: Skill
engineering = Skill { name : "Engineering", skillType : FieldSpecific, ability : Comprehension }

gunnery :: Skill
gunnery = Skill { name : "Gunnery", skillType : SingleValue, ability : Intuition }

interest :: Skill
interest = Skill { name : "Interest", skillType : FieldSpecific, ability : Comprehension }

investigation :: Skill
investigation = Skill { name : "Investigation", skillType : SingleValue, ability : Intuition }

language :: Skill
language = Skill { name : "Language", skillType : FieldSpecific, ability : Comprehension }

medicine :: Skill
medicine = Skill { name : "Medicine", skillType : FieldSpecific, ability : Comprehension }

melee :: Skill
melee = Skill { name : "Melee", skillType : FieldSpecific, ability : Strength }

navigation :: Skill
navigation = Skill { name : "Navigation", skillType : SingleValue, ability : Intuition }

operations :: Skill
operations = Skill { name : "Operations", skillType : SingleValue, ability : Comprehension }

perception :: Skill
perception = Skill { name : "Perception", skillType : SingleValue, ability : Intuition }

persuasion :: Skill
persuasion = Skill { name : "Persuasion", skillType : SingleValue, ability : Intuition }

pilot :: Skill
pilot = Skill { name : "Pilot", skillType : FieldSpecific, ability : Agility }

profession :: Skill
profession = Skill { name : "Profession", skillType : FieldSpecific, ability : Comprehension }

programming :: Skill
programming = Skill { name : "Programming", skillType : SingleValue, ability : Comprehension }

protocol :: Skill
protocol = Skill { name : "Protocol", skillType : SingleValue, ability : Intuition }

rangedWeapons :: Skill
rangedWeapons = Skill { name : "RangedWeapons", skillType : FieldSpecific, ability : Agility }

research :: Skill
research = Skill { name : "Research", skillType : SingleValue, ability : Comprehension }

stealth :: Skill
stealth = Skill { name : "Stealth", skillType : SingleValue, ability : Agility }

survival :: Skill
survival = Skill { name : "Survival", skillType : SingleValue, ability : Intuition }

thrownWeapons :: Skill
thrownWeapons = Skill { name : "ThrownWeapons", skillType : SingleValue, ability : Strength }

zeroG :: Skill
zeroG = Skill { name : "Zero-G", skillType : SingleValue, ability : Agility }

skills :: Array Skill
skills =
  [ academics
  , art
  , athletics
  , deception
  , demolitions
  , disguise
  , engineering
  , gunnery
  , interest
  , investigation
  , language
  , medicine
  , melee
  , navigation
  , operations
  , perception
  , persuasion
  , pilot
  , profession
  , programming
  , protocol
  , rangedWeapons
  , research
  , stealth
  , survival
  , thrownWeapons
  , zeroG
  ]
