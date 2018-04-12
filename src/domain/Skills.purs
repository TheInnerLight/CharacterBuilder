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

type SkillBoundaries = M.Map Skill (Tuple3 Int Int Int)

data FreeSkillBonus
  = SpecificSkill Skill Int
  | OneOfTwoSkills Skill Skill Int
  | OneOfThreeSkills Skill Skill Skill Int

derive instance newtypeSkill :: Newtype Skill _
derive instance eqSkill :: Eq Skill
derive instance ordSkill :: Ord Skill
derive instance eqSkillType :: Eq SkillType
derive instance ordSkillType :: Ord SkillType

costOfBuyingSkill :: SkillBoundaries -> Skill -> Int -> Int
costOfBuyingSkill boundaries skill value =
  let boundary = fromMaybe (tuple3 3 6 9) $ M.lookup skill $ boundaries in 
  uncurry3 (costWithBounds value) boundary
  where
  costWithBounds value bound1 bound2 bound3 = 
    case value of
      v | v < bound1 -> v
      v | v < bound2 -> bound1 + (v - bound1) * 2
      v | v < bound3 -> bound1 + (bound2 - bound1) * 2 + (v - bound2) * 3
      v              -> bound1 + (bound2 - bound1) * 2 + (bound3 - bound2) * 3 + (v - bound3) * 4

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

thrownWeapons :: Skill
thrownWeapons = Skill { name : "ThrownWeapons", skillType : SingleValue, ability : Strength }

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
  , thrownWeapons
  ]
