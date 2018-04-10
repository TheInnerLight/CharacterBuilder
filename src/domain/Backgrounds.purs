module Background where

import Data.Either
import Data.Foldable
import Data.Newtype
import Data.Tuple
import Prelude
import Skills

import Data.Either.Nested (Either3, either3)
import Data.Map as M
import Data.Set as S
import SkillMap as SM

data FreeSkillBonus
  = SpecificSkill Skill Int
  | OneOfTwoSkills Skill Skill Int
  | OneOfThreeSkills Skill Skill Skill Int

type Background = 
  { startingSkills :: SM.SkillMap
  , name :: String
  , freeSkillBonuses :: Array FreeSkillBonus
  }

cp :: forall t a. Newtype t a => (a -> a) -> t -> t
cp f n = wrap $ f $ unwrap n

daystromInstitute :: Background
daystromInstitute = 
  { startingSkills : skills
  , name : "Daystrom Institute"
  , freeSkillBonuses : []
  }
  where 
  skills =
    foldr (\(Tuple k v) -> M.insert k v) (SM.empty)
    [ SM.relatedSkills academics $ M.fromFoldable [Tuple "AI" 2, Tuple "Computer Science" 4]
    , SM.singleSkill operations 2
    , SM.singleSkill programming 3
    , SM.singleSkill research 3
    ]
    -- S.fromFoldable
    -- [ cp (\x -> x { rating = FieldSpecific $ M.fromFoldable [Tuple "AI" 2, Tuple "Computer Science" 4] }) academics
    -- , cp (\x -> x { rating = SingleValue 2 }) operations
    -- , cp (\x -> x { rating = SingleValue 3 }) programming
    -- , cp (\x -> x { rating = SingleValue 3 }) research
    -- ]

formerMaquis ::  Background
formerMaquis = 
  { startingSkills : skills
  , name : "Former Maquis"
  , freeSkillBonuses : [SpecificSkill academics 2, SpecificSkill profession 2]
  }
  where 
  skills = 
    foldr (\(Tuple k v) -> M.insert k v) (SM.empty)
    [ SM.singleSkill demolitions 2
    , SM.relatedSkills melee $ M.fromFoldable [Tuple "Unarmed" 2]
    , SM.relatedSkills pilot  $ M.fromFoldable [Tuple "Spacecraft" 2]
    , SM.relatedSkills rangedWeapons  $ M.fromFoldable [Tuple "Phasers" 2]
    ]

klingonDefenseAcademy :: Background
klingonDefenseAcademy = 
  { startingSkills : skills
  , name : "Klingon Defense Academcy" 
  , freeSkillBonuses : [OneOfTwoSkills academics engineering 2, OneOfTwoSkills academics engineering 2]
  }
  where 
  skills =
    foldr (\(Tuple k v) -> M.insert k v) (SM.empty)
    [ SM.singleSkill gunnery 2
    , SM.relatedSkills melee $ M.fromFoldable [Tuple "Unarmed" 2, Tuple "Bladed" 4]
    , SM.relatedSkills profession $ M.fromFoldable [Tuple "Soldier" 2]
    , SM.relatedSkills rangedWeapons $ M.fromFoldable [Tuple "Disruptors" 2]
    ]

liberatedDrone :: Background
liberatedDrone = 
  { startingSkills : skills
  , name : "Liberated Drone"
  , freeSkillBonuses : [SpecificSkill academics 2, SpecificSkill academics 2, SpecificSkill academics 2]
  }
  where 
  skills =
    foldr (\(Tuple k v) -> M.insert k v) (SM.empty)
    [ SM.singleSkill operations 2
    , SM.singleSkill programming 3
    , SM.singleSkill research 3
    ]

romulanWarCollege :: Background
romulanWarCollege = 
  { startingSkills : skills
  , name : "Romulan War College"
  , freeSkillBonuses : [OneOfThreeSkills academics engineering medicine 2, OneOfThreeSkills academics engineering medicine 2, OneOfThreeSkills academics engineering medicine 2]
  }
  where 
  skills =
    foldr (\(Tuple k v) -> M.insert k v) (SM.empty)
    [ SM.singleSkill investigation 2
    , SM.singleSkill operations 2
    , SM.relatedSkills profession $ M.fromFoldable [Tuple "Soldier" 2]
    , SM.relatedSkills rangedWeapons $ M.fromFoldable [Tuple "Disruptors" 2]
    ]

section31 :: Background
section31 = 
  { startingSkills : skills
  , name : "Section 31"
  , freeSkillBonuses : [OneOfTwoSkills academics engineering 2]
  }
  where 
  skills =
    foldr (\(Tuple k v) -> M.insert k v) (SM.empty)
    [ SM.relatedSkills academics $ M.fromFoldable [Tuple "Cryptography" 2]
    , SM.singleSkill deception 2
    , SM.singleSkill investigation 2
    , SM.singleSkill operations 2
    , SM.singleSkill stealth 2
    ]

starfleetAcademy :: Background
starfleetAcademy  = 
  { startingSkills : skills
  , name : "Starfleet Academy"
  , freeSkillBonuses : [OneOfThreeSkills academics engineering medicine 2, OneOfThreeSkills academics engineering medicine 2, OneOfThreeSkills academics engineering medicine 2]
  }
  where 
  skills =
    foldr (\(Tuple k v) -> M.insert k v) (SM.empty)
    [ SM.relatedSkills melee $ M.fromFoldable [Tuple "Unarmed" 2]
    , SM.relatedSkills profession $ M.fromFoldable [Tuple "Soldier" 2]
    , SM.singleSkill protocol 2
    , SM.relatedSkills rangedWeapons $ M.fromFoldable [Tuple "Phasers" 2]
    ]

vulcanScienceAcademy :: Background
vulcanScienceAcademy = 
  { startingSkills : skills
  , name : "Vulcan Science Academy"
  , freeSkillBonuses : [SpecificSkill academics 5, SpecificSkill academics 5]
  }
  where
  skills = 
    foldr (\(Tuple k v) -> M.insert k v) (SM.empty)
    [ SM.singleSkill research 4
    ]

trillScienceMinistry :: Background
trillScienceMinistry = vulcanScienceAcademy {name = "Trill Science Ministry"}

backgrounds :: Array Background
backgrounds =
  [ daystromInstitute
  , formerMaquis
  , klingonDefenseAcademy
  , liberatedDrone
  , romulanWarCollege
  , section31
  , starfleetAcademy
  , vulcanScienceAcademy
  , trillScienceMinistry
  ]