module Background where

import Prelude
import Skills
import Data.Tuple
import Data.Map as Map
import Data.Either
import Data.Either.Nested (Either3, either3)

data FreeSkillBonus
  = SpecificSkill SkillName Int
  | OneOfTwoSkills SkillName SkillName Int
  | OneOfThreeSkills SkillName SkillName SkillName Int

type Background = 
  { startingSkills :: Map.Map Skill Int
  , name :: String
  , freeSkillBonuses :: Array FreeSkillBonus
  }

daystromInstitute :: Background
daystromInstitute = 
 { startingSkills : skillMap
 , name : "Daystrom Institute"
 , freeSkillBonuses : []
 }
 where 
 skillMap =
    Map.fromFoldable
      [ Tuple (academics "AI")               2
      , Tuple (academics "Computer Science") 4
      , Tuple operations                     2
      , Tuple programming                    3
      , Tuple research                       3
      ]

formerMaquis ::  Background
formerMaquis = 
  { startingSkills : skillMap
  , name : "Former Maquis"
  , freeSkillBonuses : [SpecificSkill Academics 2, SpecificSkill Profession 2]
  }
  where 
  skillMap = Map.fromFoldable
      [ Tuple demolitions                    2
      , Tuple (melee "Unarmed")              2
      , Tuple (pilot "Spacecraft")           2
      , Tuple (rangedWeapons "Phasers")      2
      ]

klingonDefenseAcademy :: Background
klingonDefenseAcademy = 
  { startingSkills : skillMap
  , name : "Klingon Defense Academcy" 
  , freeSkillBonuses : [OneOfTwoSkills Academics Engineering 2, OneOfTwoSkills Academics Engineering 2]
  }
  where 
  skillMap = Map.fromFoldable 
      [ Tuple gunnery                        2
      , Tuple (melee "Bladed")               2
      , Tuple (melee "Unarmed")              2
      , Tuple (profession "Soldier")         2
      , Tuple (rangedWeapons "Disruptors")   2
      ]

liberatedDrone :: Background
liberatedDrone = 
  { startingSkills : skillMap
  , name : "Liberated Drone"
  , freeSkillBonuses : [SpecificSkill Academics 2, SpecificSkill Academics 2, SpecificSkill Academics 2]
  }
  where 
  skillMap = Map.fromFoldable
    [ Tuple operations                     2
    , Tuple programming                    3
    , Tuple research                       3
    ]

romulanWarCollege :: Background
romulanWarCollege = 
  { startingSkills : skillMap
  , name : "Romulan War College"
  , freeSkillBonuses : [OneOfThreeSkills Academics Engineering Medicine 2, OneOfThreeSkills Academics Engineering Medicine 2, OneOfThreeSkills Academics Engineering Medicine 2]
  }
  where 
  skillMap = Map.fromFoldable 
      [ Tuple investigation                  2
      , Tuple operations                     2
      , Tuple (profession "Soldier")         2
      , Tuple (rangedWeapons "Disruptors")   2
      ]

section31 :: Background
section31 = 
  { startingSkills : skillMap
  , name : "Section 31"
  , freeSkillBonuses : [OneOfTwoSkills Academics Engineering 2]
  }
  where 
  skillMap = Map.fromFoldable 
      [ Tuple (academics "Cryptography")     2
      , Tuple deception                      2
      , Tuple investigation                  2
      , Tuple operations                     2
      , Tuple stealth                        2
      ]

starfleetAcademy :: Background
starfleetAcademy  = 
  { startingSkills : skillMap
  , name : "Starfleet Academy"
  , freeSkillBonuses : [OneOfThreeSkills Academics Engineering Medicine 2, OneOfThreeSkills Academics Engineering Medicine 2, OneOfThreeSkills Academics Engineering Medicine 2]
  }
  where 
  skillMap = Map.fromFoldable 
      [ Tuple (melee "Unarmed")              2
      , Tuple (profession "Soldier")         2
      , Tuple (protocol)                     2
      , Tuple (rangedWeapons "Phasers")      2
      ]

vulcanScienceAcademy :: Background
vulcanScienceAcademy = 
  { startingSkills : skillMap
  , name : "Vulcan Science Academy"
  , freeSkillBonuses : [SpecificSkill Academics 5, SpecificSkill Academics 5]
  }
  where
  skillMap = Map.fromFoldable 
    [ Tuple research                         4
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