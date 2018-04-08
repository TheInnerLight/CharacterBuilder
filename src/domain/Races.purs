module Races where

import Data.Tuple
import Data.Tuple.Nested
import Prelude

import Abilities (Ability(..))
import Data.Map as Map
import Skills (SkillName(..))

data Advantage 
  = AbilityBonus Ability Int
  | AnyAbilityBonus Int
  | Telepathy
  | Unreadable

instance showAdvantage :: Show Advantage where
  show (AbilityBonus ability bonus) = show bonus <> " bonus to " <> show ability <> " score."
  show (AnyAbilityBonus amount)     = show amount <> " bonus to any ability score(s)."
  show Telepathy                    = "Telepathy."
  show Unreadable                   = "Immune to telepathy."

data Disadvantage 
  = NoAutomaticLanguage
  | IllegalGeneticStatus

instance showDisadvantage :: Show Disadvantage where
  show NoAutomaticLanguage  = "Does not start with any skills a language automatically."
  show IllegalGeneticStatus = "There may be negative consequences if your genetic status is discovered."

type Race =
  { skillBoundaries :: Map.Map SkillName (Tuple3 Int Int Int)
  , fate :: Int
  , advantages :: Array Advantage
  , disadvantages :: Array Disadvantage
  , name :: String
  }

increasedBy :: Int -> Tuple3 Int Int Int 
increasedBy x = tuple3 (3 + x) (6 + x) (9 + x)

andorian :: Race 
andorian = 
  { skillBoundaries : Map.fromFoldable 
    [ Tuple Art           (increasedBy 1)
    , Tuple Academics     (increasedBy 1)
    , Tuple Protocol      (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Andorian"
  }

android :: Race 
android =
  { skillBoundaries : Map.empty
  , fate : 1
  , advantages : [AbilityBonus Strength 1, AbilityBonus Comprehension 1, AbilityBonus Health 1, Unreadable]
  , disadvantages : []
  , name : "Android"
  }

bajoran :: Race 
bajoran =
  { skillBoundaries : Map.fromFoldable 
    [ Tuple Art           (increasedBy 1)
    , Tuple Academics     (increasedBy 1)
    , Tuple Athletics     (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Bajoran"
  }

betazoid :: Race 
betazoid =
  { skillBoundaries : Map.empty
  , fate : 2
  , advantages : [Telepathy]
  , disadvantages : []
  , name : "Betazoid"
  }

benzite :: Race 
benzite =
  { skillBoundaries : Map.fromFoldable 
    [ Tuple Investigation (increasedBy 1)
    , Tuple Protocol      (increasedBy 1)
    , Tuple Research      (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Benzite"
  }

bolian :: Race 
bolian =
  { skillBoundaries : Map.fromFoldable 
    [ Tuple Art           (increasedBy 1)
    , Tuple Perception    (increasedBy 1)
    , Tuple Persuasion    (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Bolian"
  }

cairn :: Race 
cairn =
  { skillBoundaries : Map.fromFoldable 
    [ Tuple Perception    (increasedBy 1)]
  , fate : 2
  , advantages : [Telepathy]
  , disadvantages : [NoAutomaticLanguage]
  , name : "Cairn"
  }

cardassian :: Race 
cardassian =
  { skillBoundaries : Map.fromFoldable 
    [ Tuple Deception     (increasedBy 1)
    , Tuple Profession    (increasedBy 1)
    , Tuple Investigation (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Cardassian"
  }

deltan :: Race 
deltan =
  { skillBoundaries : Map.fromFoldable 
    [ Tuple Academics     (increasedBy 1)
    , Tuple Art           (increasedBy 1)]
  , fate : 1
  , advantages : [AbilityBonus Comprehension 1, AbilityBonus Intuition 1]
  , disadvantages : []
  , name : "Deltan"
  }

efrosian :: Race
efrosian =
  { skillBoundaries : Map.fromFoldable 
    [ Tuple Melee         (increasedBy 1)
    , Tuple Profession    (increasedBy 1)]
  , fate : 2
  , advantages : [AbilityBonus Strength 1]
  , disadvantages : []
  , name : "Efrosian"
  }

ferengi :: Race
ferengi =
  { skillBoundaries : Map.fromFoldable 
    [ Tuple Profession    (increasedBy 1)
    , Tuple Perception    (increasedBy 2)]
  , fate : 3
  , advantages : [Unreadable]
  , disadvantages : []
  , name : "Ferengi"
  }

grazerite :: Race
grazerite =
  { skillBoundaries : Map.fromFoldable 
    [ Tuple Profession    (increasedBy 1)
    , Tuple Perception    (increasedBy 1)
    , Tuple Protocol      (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Grazerite"
  }

human :: Race
human =
  { skillBoundaries : Map.fromFoldable 
    [ Tuple Profession    (increasedBy 1)
    , Tuple Protocol      (increasedBy 1)
    , Tuple Research      (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Human"
  }

geneticallyEngineeredHuman :: Race
geneticallyEngineeredHuman =
  { skillBoundaries : Map.empty
  , fate : 2
  , advantages : [AnyAbilityBonus 2]
  , disadvantages : [IllegalGeneticStatus]
  , name : "Genetically Engineered Human"
  }

klingon :: Race
klingon =
  { skillBoundaries : Map.fromFoldable 
    [ Tuple Athletics     (increasedBy 1)
    , Tuple Gunnery       (increasedBy 1)
    , Tuple Melee         (increasedBy 1)]
  , fate : 2
  , advantages : [AbilityBonus Strength 1]
  , disadvantages : []
  , name : "Klingon"
  }

romulan :: Race
romulan =
  { skillBoundaries : Map.fromFoldable 
    [ Tuple Academics     (increasedBy 1)
    , Tuple Deception     (increasedBy 1)
    , Tuple Investigation (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Romulan"
  }

tellarite :: Race
tellarite =
  { skillBoundaries : Map.fromFoldable 
    [ Tuple Engineering   (increasedBy 1)
    , Tuple Profession    (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Tellarite"
  }

trill :: Race
trill =
  { skillBoundaries : Map.fromFoldable 
    [ Tuple Academics     (increasedBy 1)
    , Tuple Profession    (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Trill"
  }

joinedTrill :: Race
joinedTrill =
  { skillBoundaries : Map.fromFoldable 
    [ Tuple Academics     (increasedBy 1)
    , Tuple Profession    (increasedBy 1)]
  , fate : 1
  , advantages : [AnyAbilityBonus 2]
  , disadvantages : []
  , name : "Joined Trill"
  }

vulcan :: Race 
vulcan =
  { skillBoundaries : Map.fromFoldable 
    [ Tuple Academics     (increasedBy 1)]
  , fate : 1
  , advantages : [AbilityBonus Strength 1, AbilityBonus Comprehension 1, AbilityBonus Resolve 1]
  , disadvantages : []
  , name : "Vulcan"
  }

zakdorn :: Race
zakdorn = 
  { skillBoundaries : Map.fromFoldable 
    [ Tuple Investigation (increasedBy 1)
    , Tuple Perception    (increasedBy 1)
    , Tuple Profession    (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Zakdorn"
  }

races :: Array Race
races = 
  [ andorian
  , android
  , bajoran
  , betazoid
  , bolian
  , cairn
  , cardassian
  , deltan
  , efrosian
  , ferengi
  , grazerite
  , human
  , geneticallyEngineeredHuman
  , klingon
  , romulan
  , tellarite
  , trill
  , joinedTrill
  , vulcan
  , zakdorn
  ]