module Skills where

import Prelude
import Abilities

data SkillType 
    = FieldSpecific String 
    | Single

data SkillName
    = Academics
    | Acrobatics
    | Art
    | Athletics
    | Deception
    | Demolitions
    | Disguise
    | Engineering
    | Gunnery
    | Interest
    | Investigation
    | Language
    | Medicine
    | Melee
    | Navigation
    | Operations
    | Perception
    | Persuasion
    | Pilot
    | Profession
    | Programming
    | Protocol
    | RangedWeapons
    | Research
    | Stealth
    | ThrownWeapons

newtype Skill = Skill
    { name :: SkillName
    , type :: SkillType
    , ability :: Ability
    }

derive instance eqSkillName :: Eq SkillName
derive instance ordSkillName :: Ord SkillName
derive instance eqSkillType :: Eq SkillType
derive instance ordSkillType :: Ord SkillType
derive instance eqSkill :: Eq Skill
derive instance ordSkill :: Ord Skill

academics :: String -> Skill
academics name = Skill { name : Academics, type : FieldSpecific name, ability : Comprehension }

acrobatics :: Skill
acrobatics = Skill { name : Acrobatics, type : Single, ability : Agility }

art :: String -> Skill
art name = Skill { name : Art, type : FieldSpecific name, ability : Intuition }

athletics :: Skill
athletics = Skill { name : Athletics, type : Single, ability : Strength }

deception :: Skill
deception = Skill { name : Deception, type : Single, ability : Intuition }

demolitions :: Skill
demolitions = Skill { name : Demolitions, type : Single, ability : Comprehension }

diguise :: Skill
diguise = Skill { name : Disguise, type : Single, ability : Intuition }

engineering :: String -> Skill
engineering name = Skill { name : Engineering, type : FieldSpecific name, ability : Comprehension }

gunnery :: Skill
gunnery = Skill { name : Gunnery, type : Single, ability : Intuition }

interest :: String -> Skill
interest name = Skill { name : Interest, type : FieldSpecific name, ability : Comprehension }

investigation :: Skill
investigation = Skill { name : Investigation, type : Single, ability : Intuition }

language :: String -> Skill
language name = Skill { name : Language, type : FieldSpecific name, ability : Comprehension }

medicine :: String -> Skill
medicine name = Skill { name : Medicine, type : FieldSpecific name, ability : Comprehension }

melee :: String -> Skill
melee name = Skill { name : Melee, type : FieldSpecific name, ability : Strength }

navigation :: Skill
navigation = Skill { name : Navigation, type : Single, ability : Intuition }

operations :: Skill
operations = Skill { name : Operations, type : Single, ability : Comprehension }

perception :: Skill
perception = Skill { name : Perception, type : Single, ability : Intuition }

persuasion :: Skill
persuasion = Skill { name : Persuasion, type : Single, ability : Intuition }

pilot :: String -> Skill
pilot name = Skill { name : Pilot, type : FieldSpecific name, ability : Agility }

profession :: String -> Skill
profession name = Skill { name : Profession, type : FieldSpecific name, ability : Comprehension }

programming :: Skill
programming = Skill { name : Programming, type : Single, ability : Comprehension }

protocol :: Skill
protocol = Skill { name : Protocol, type : Single, ability : Intuition }

rangedWeapons :: String -> Skill
rangedWeapons name = Skill { name : RangedWeapons, type : FieldSpecific name, ability : Agility }

research :: Skill
research = Skill { name : Research, type : Single, ability : Comprehension }

stealth :: Skill
stealth = Skill { name : Stealth, type : Single, ability : Agility }

thrownWeapons :: Skill
thrownWeapons = Skill { name : ThrownWeapons, type : Single, ability : Strength }
