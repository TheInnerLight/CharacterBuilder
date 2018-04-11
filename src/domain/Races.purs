module Races where

import Data.Tuple
import Data.Tuple.Nested
import Prelude
import Skills

import Abilities (Ability(..))
import Data.Map as M

data Advantage 
  = AbilityBonus Ability Int
  | AnyAbilityBonus Int
  | Telepathy
  | Unreadable

instance showAdvantage :: Show Advantage where
  show (AbilityBonus ability bonus) = "+" <> show bonus <> " bonus to " <> show ability <> " score."
  show (AnyAbilityBonus amount)     = "+" <> show amount <> " bonus to any ability score(s)."
  show Telepathy                    = "Telepathy."
  show Unreadable                   = "Immune to telepathy."

data Disadvantage 
  = NoAutomaticLanguage
  | IllegalGeneticStatus

instance showDisadvantage :: Show Disadvantage where
  show NoAutomaticLanguage  = "No starting language skills."
  show IllegalGeneticStatus = "Illegal Genetic Status."

type Race =
  { skillBoundaries :: M.Map Skill (Tuple3 Int Int Int)
  , fate :: Int
  , advantages :: Array Advantage
  , disadvantages :: Array Disadvantage
  , name :: String
  }

increasedBy :: Int -> Tuple3 Int Int Int 
increasedBy x = tuple3 (3 + x) (6 + x) (9 + x)

andorian :: Race 
andorian = 
  { skillBoundaries :
    M.fromFoldable
    [ Tuple art           (increasedBy 1)
    , Tuple academics     (increasedBy 1)
    , Tuple protocol      (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Andorian"
  }

android :: Race 
android =
  { skillBoundaries : M.fromFoldable []
  , fate : 1
  , advantages : [AbilityBonus Strength 1, AbilityBonus Comprehension 1, AbilityBonus Health 1, Unreadable]
  , disadvantages : []
  , name : "Android"
  }

bajoran :: Race 
bajoran =
  { skillBoundaries : 
    M.fromFoldable
    [ Tuple art           (increasedBy 1)
    , Tuple academics     (increasedBy 1)
    , Tuple athletics     (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Bajoran"
  }

betazoid :: Race 
betazoid =
  { skillBoundaries : M.fromFoldable []
  , fate : 2
  , advantages : [Telepathy]
  , disadvantages : []
  , name : "Betazoid"
  }

benzite :: Race 
benzite =
  { skillBoundaries :
    M.fromFoldable 
    [ Tuple investigation (increasedBy 1)
    , Tuple protocol      (increasedBy 1)
    , Tuple research      (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Benzite"
  }

bolian :: Race 
bolian =
  { skillBoundaries : 
    M.fromFoldable
    [ Tuple art           (increasedBy 1)
    , Tuple perception    (increasedBy 1)
    , Tuple persuasion    (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Bolian"
  }

cairn :: Race 
cairn =
  { skillBoundaries :  
    M.fromFoldable
    [ Tuple perception    (increasedBy 1)]
  , fate : 2
  , advantages : [Telepathy]
  , disadvantages : [NoAutomaticLanguage]
  , name : "Cairn"
  }

cardassian :: Race 
cardassian =
  { skillBoundaries :  
    M.fromFoldable
    [ Tuple deception     (increasedBy 1)
    , Tuple profession    (increasedBy 1)
    , Tuple investigation (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Cardassian"
  }

deltan :: Race 
deltan =
  { skillBoundaries : 
    M.fromFoldable
    [ Tuple academics     (increasedBy 1)
    , Tuple art           (increasedBy 1)]
  , fate : 1
  , advantages : [AbilityBonus Comprehension 1, AbilityBonus Intuition 1]
  , disadvantages : []
  , name : "Deltan"
  }

efrosian :: Race
efrosian =
  { skillBoundaries : 
    M.fromFoldable
    [ Tuple melee         (increasedBy 1)
    , Tuple profession    (increasedBy 1)]
  , fate : 2
  , advantages : [AbilityBonus Strength 1]
  , disadvantages : []
  , name : "Efrosian"
  }

ferengi :: Race
ferengi =
  { skillBoundaries : 
    M.fromFoldable
    [ Tuple profession    (increasedBy 1)
    , Tuple perception    (increasedBy 2)]
  , fate : 3
  , advantages : [Unreadable]
  , disadvantages : []
  , name : "Ferengi"
  }

grazerite :: Race
grazerite =
  { skillBoundaries :
    M.fromFoldable
    [ Tuple profession    (increasedBy 1)
    , Tuple perception    (increasedBy 1)
    , Tuple protocol      (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Grazerite"
  }

human :: Race
human =
  { skillBoundaries : 
    M.fromFoldable
    [ Tuple profession    (increasedBy 1)
    , Tuple protocol      (increasedBy 1)
    , Tuple research      (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Human"
  }

geneticallyEngineeredHuman :: Race
geneticallyEngineeredHuman =
  { skillBoundaries : M.fromFoldable []
  , fate : 2
  , advantages : [AnyAbilityBonus 2]
  , disadvantages : [IllegalGeneticStatus]
  , name : "Genetically Engineered Human"
  }

klingon :: Race
klingon =
  { skillBoundaries : 
    M.fromFoldable
    [ Tuple athletics     (increasedBy 1)
    , Tuple gunnery       (increasedBy 1)
    , Tuple melee         (increasedBy 1)]
  , fate : 2
  , advantages : [AbilityBonus Strength 1]
  , disadvantages : []
  , name : "Klingon"
  }

romulan :: Race
romulan =
  { skillBoundaries :  
    M.fromFoldable
    [ Tuple academics     (increasedBy 1)
    , Tuple deception     (increasedBy 1)
    , Tuple investigation (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Romulan"
  }

tellarite :: Race
tellarite =
  { skillBoundaries : 
    M.fromFoldable
    [ Tuple engineering   (increasedBy 1)
    , Tuple profession    (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Tellarite"
  }

trill :: Race
trill =
  { skillBoundaries : 
    M.fromFoldable
    [ Tuple academics     (increasedBy 1)
    , Tuple profession    (increasedBy 1)]
  , fate : 3
  , advantages : []
  , disadvantages : []
  , name : "Trill"
  }

joinedTrill :: Race
joinedTrill =
  { skillBoundaries :
    M.fromFoldable
    [ Tuple academics     (increasedBy 1)
    , Tuple profession    (increasedBy 1)]
  , fate : 1
  , advantages : [AnyAbilityBonus 2]
  , disadvantages : []
  , name : "Joined Trill"
  }

vulcan :: Race 
vulcan =
  { skillBoundaries :
    M.fromFoldable
    [ Tuple academics     (increasedBy 1)]
  , fate : 1
  , advantages : [AbilityBonus Strength 1, AbilityBonus Comprehension 1, AbilityBonus Resolve 1]
  , disadvantages : []
  , name : "Vulcan"
  }

zakdorn :: Race
zakdorn = 
  { skillBoundaries :
    M.fromFoldable
    [ Tuple investigation (increasedBy 1)
    , Tuple perception    (increasedBy 1)
    , Tuple profession    (increasedBy 1)]
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