module Background where

import Prelude
import Skills
import Data.Tuple
import Data.Map as Map
import Data.Either
import Data.Either.Nested (Either3, either3)

data Background 
  = DaystromInstitute
  | FormerMaquis
  | KlingonDefenseAcademy
  | LiberateDrone
  | RomulanWarCollege
  | Section31
  | StarfleetAcademy
  | VulcanScienceAcademy
  | TrillScienceMinistry

applyAcademicsOrEngineeringOfChoice :: Either String String -> Map.Map Skill Int -> Map.Map Skill Int
applyAcademicsOrEngineeringOfChoice (Left academicsField) base = Map.insert (academics academicsField) 2 base
applyAcademicsOrEngineeringOfChoice (Right engineeringField) base = Map.insert (engineering engineeringField) 2 base

applyAcademicsEngineeringOrMedicineOfChoice :: Either3 String String String -> Map.Map Skill Int -> Map.Map Skill Int
applyAcademicsEngineeringOrMedicineOfChoice e3 base = 
  either3
    (\academicsField   -> Map.insert (academics academicsField) 2 base) 
    (\engineeringField -> Map.insert (engineering engineeringField) 2 base)
    (\medicideField    -> Map.insert (medicine medicideField) 2 base)
    e3

daystromInstitute :: Map.Map Skill Int
daystromInstitute = Map.fromFoldable
  [ Tuple (academics "AI")               2
  , Tuple (academics "Computer Science") 4
  , Tuple operations                     2
  , Tuple programming                    3
  , Tuple research                       3
  ]

formerMaquis :: String -> String -> Map.Map Skill Int
formerMaquis academicsField professionField = Map.fromFoldable
  [ Tuple (academics academicsField)     2
  , Tuple demolitions                    2
  , Tuple (melee "Unarmed")              2
  , Tuple (pilot "Spacecraft")           2
  , Tuple (profession professionField)   2
  , Tuple (rangedWeapons "Phasers")      2
  ]

klingonDefenseAcademy :: Either String String -> Either String String -> Map.Map Skill Int
klingonDefenseAcademy ea1 ea2 = applyAcademicsOrEngineeringOfChoice ea1 <<< applyAcademicsOrEngineeringOfChoice ea2 $ base
  where 
  base = Map.fromFoldable 
      [ Tuple gunnery                        2
      , Tuple (melee "Bladed")               2
      , Tuple (melee "Unarmed")              2
      , Tuple (profession "Soldier")         2
      , Tuple (rangedWeapons "Disruptors")   2
      ]

liberatedDrone :: String -> String -> String -> Map.Map Skill Int 
liberatedDrone academicsField1 academicsField2 academicsField3 = Map.fromFoldable
  [ Tuple (academics academicsField1)    2
  , Tuple (academics academicsField2)    2
  , Tuple (academics academicsField3)    2
  , Tuple operations                     2
  , Tuple programming                    3
  , Tuple research                       3
]

romulanWarCollege :: Either3 String String String -> Either3 String String String -> Either3 String String String -> Map.Map Skill Int
romulanWarCollege eam1 eam2 eam3 = applyAcademicsEngineeringOrMedicineOfChoice eam1 <<< applyAcademicsEngineeringOrMedicineOfChoice eam2 <<< applyAcademicsEngineeringOrMedicineOfChoice eam3 $ base
  where 
  base = Map.fromFoldable 
      [ Tuple investigation                  2
      , Tuple operations                     2
      , Tuple (profession "Soldier")         2
      , Tuple (rangedWeapons "Disruptors")   2
      ]

section31 :: Either String String -> Map.Map Skill Int
section31 ae = applyAcademicsOrEngineeringOfChoice ae base
  where 
  base = Map.fromFoldable 
      [ Tuple (academics "Cryptography")     2
      , Tuple deception                      2
      , Tuple investigation                  2
      , Tuple operations                     2
      , Tuple stealth                        2
      ]

starfleetAcademy :: Either3 String String String -> Either3 String String String -> Either3 String String String -> Map.Map Skill Int
starfleetAcademy eam1 eam2 eam3 = applyAcademicsEngineeringOrMedicineOfChoice eam1 <<< applyAcademicsEngineeringOrMedicineOfChoice eam2 <<< applyAcademicsEngineeringOrMedicineOfChoice eam3 $ base
  where 
  base = Map.fromFoldable 
      [ Tuple (melee "Unarmed")              2
      , Tuple (profession "Soldier")         2
      , Tuple (protocol)                     2
      , Tuple (rangedWeapons "Phasers")      2
      ]

scienceAcademy :: String -> String -> Map.Map Skill Int
scienceAcademy a1 a2 = Map.fromFoldable 
  [ Tuple (academics a1)                 5
  , Tuple (academics a2)                 5
  , Tuple research                       4
  ]