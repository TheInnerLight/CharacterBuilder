module Character where

import Abilities
import Data.List
import Data.Map
import Prelude
import Skills

import Background (Background)
import Races (Race(..))

type Character = 
  { abilities :: PrimaryAbilities
  , skills :: Map Skill Int
  , race :: Race
  , background :: Background
  }