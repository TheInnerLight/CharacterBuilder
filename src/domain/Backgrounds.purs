module Background where

import Data.Either
import Data.Foldable
import Data.Newtype
import Data.Tuple
import Prelude
import Skills
import Skills
import Data.Either.Nested (Either3, either3)
import Data.Map as M
import Data.Set as S
import SkillMap as SM

type Background = 
  { startingSkills :: SM.SkillMap
  , name :: String
  , freeSkillBonuses :: Array FreeSkillBonus
  }

cp :: forall t a. Newtype t a => (a -> a) -> t -> t
cp f n = wrap $ f $ unwrap n

andorianAcademy :: Background
andorianAcademy = 
  { startingSkills : skills
  , name : "Andorian Academy"
  , freeSkillBonuses : [SpecificSkill academics 4, SpecificSkill art 5]
  }
  where
  skills = 
    foldr (\(Tuple k v) -> M.insert k v) (SM.empty)
    [ SM.singleSkill research 4
    , SM.singleSkill operations 2
    , SM.singleSkill protocol 3
    ]

bajoranResistance ::  Background
bajoranResistance = 
  { startingSkills : skills
  , name : "Bajoran Resistance"
  , freeSkillBonuses : [SpecificSkill engineering 2, SpecificSkill profession 2]
  }
  where 
  skills = 
    foldr (\(Tuple k v) -> M.insert k v) (SM.empty)
    [ SM.singleSkill deception 2
    , SM.singleSkill demolitions 4
    , SM.relatedSkills language  $ M.fromFoldable [Tuple "Cardassian" 2]
    , SM.relatedSkills melee $ M.fromFoldable [Tuple "Unarmed" 2]
    , SM.singleSkill operations 3
    , SM.relatedSkills rangedWeapons  $ M.fromFoldable [Tuple "Disruptors" 3]
    , SM.singleSkill survival 3
    ]

daystromInstitute :: Background
daystromInstitute = 
  { startingSkills : skills
  , name : "Daystrom Institute"
  , freeSkillBonuses : [SpecificSkill academics 2]
  }
  where 
  skills =
    foldr (\(Tuple k v) -> M.insert k v) (SM.empty)
    [ SM.relatedSkills academics $ M.fromFoldable [Tuple "AI" 3, Tuple "Computer Science" 5]
    , SM.singleSkill operations 2
    , SM.singleSkill programming 3
    , SM.singleSkill research 4
    ]

formerMaquis ::  Background
formerMaquis = 
  { startingSkills : skills
  , name : "Former Maquis"
  , freeSkillBonuses : [SpecificSkill academics 2, SpecificSkill engineering 2, SpecificSkill profession 4]
  }
  where 
  skills = 
    foldr (\(Tuple k v) -> M.insert k v) (SM.empty)
    [ SM.singleSkill demolitions 4
    , SM.relatedSkills melee $ M.fromFoldable [Tuple "Unarmed" 2]
    , SM.relatedSkills pilot  $ M.fromFoldable [Tuple "Spacecraft" 3]
    , SM.relatedSkills rangedWeapons  $ M.fromFoldable [Tuple "Phasers" 3]
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
    , SM.relatedSkills profession $ M.fromFoldable [Tuple "Soldier" 4]
    , SM.relatedSkills rangedWeapons $ M.fromFoldable [Tuple "Disruptors" 2]
    , SM.singleSkill survival 2
    ]

liberatedDrone :: Background
liberatedDrone = 
  { startingSkills : skills
  , name : "Liberated Drone"
  , freeSkillBonuses : [SpecificSkill academics 3, SpecificSkill academics 3, SpecificSkill academics 3, SpecificSkill engineering 2]
  }
  where 
  skills =
    foldr (\(Tuple k v) -> M.insert k v) (SM.empty)
    [ SM.singleSkill operations 4
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
    [ SM.singleSkill deception 2
    , SM.singleSkill investigation 3
    , SM.singleSkill operations 3
    , SM.relatedSkills profession $ M.fromFoldable [Tuple "Soldier" 3]
    , SM.relatedSkills rangedWeapons $ M.fromFoldable [Tuple "Disruptors" 3]
    , SM.singleSkill survival 2
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
    [ SM.relatedSkills academics $ M.fromFoldable [Tuple "Cryptography" 4]
    , SM.singleSkill deception 4
    , SM.singleSkill investigation 4
    , SM.singleSkill operations 2
    , SM.singleSkill stealth 2
    , SM.singleSkill research 1
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
    , SM.singleSkill operations 2
    , SM.relatedSkills profession $ M.fromFoldable [Tuple "Soldier" 3]
    , SM.singleSkill protocol 3
    , SM.relatedSkills rangedWeapons $ M.fromFoldable [Tuple "Phasers" 2]
    , SM.singleSkill survival 2
    , SM.singleSkill zeroG 2
    ]

starfleetIntelligence :: Background
starfleetIntelligence = 
  { startingSkills : skills
  , name : "Starfleet Intelligence"
  , freeSkillBonuses : [OneOfTwoSkills academics engineering 2]
  }
  where 
  skills =
    foldr (\(Tuple k v) -> M.insert k v) (SM.empty)
    [ SM.relatedSkills academics $ M.fromFoldable [Tuple "Cryptography" 2]
    , SM.singleSkill deception 2
    , SM.singleSkill investigation 4
    , SM.singleSkill operations 3
    , SM.singleSkill programming 3
    , SM.singleSkill stealth 3
    , SM.singleSkill research 2
    ]

starfleetMedicalAcademy :: Background
starfleetMedicalAcademy  = 
  { startingSkills : skills
  , name : "Starfleet Medical Academy"
  , freeSkillBonuses : [SpecificSkill medicine 4, SpecificSkill medicine 4, OneOfThreeSkills academics engineering medicine 2]
  }
  where 
  skills =
    foldr (\(Tuple k v) -> M.insert k v) (SM.empty)
    [ SM.singleSkill operations 2
    , SM.relatedSkills profession $ M.fromFoldable [Tuple "Soldier" 2]
    , SM.singleSkill protocol 2
    , SM.relatedSkills rangedWeapons $ M.fromFoldable [Tuple "Phasers" 2]
    , SM.singleSkill survival 2
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
    , SM.singleSkill operations 3
    ]

trillScienceMinistry :: Background
trillScienceMinistry = vulcanScienceAcademy {name = "Trill Science Ministry"}

backgrounds :: Array Background
backgrounds =
  [ andorianAcademy 
  , daystromInstitute
  , formerMaquis
  , klingonDefenseAcademy
  , liberatedDrone
  , romulanWarCollege
  , section31
  , starfleetAcademy
  , starfleetIntelligence
  , starfleetMedicalAcademy
  , vulcanScienceAcademy
  , trillScienceMinistry
  ]