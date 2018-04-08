module Main where

import Abilities
import CharacterBuilder
import Data.Either
import Data.List
import Data.Maybe
import Data.Tuple
import Prelude
import RaceSelector
import AbilitySelector
import Races
import Skills

import Background (Background)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM) as DOM
import Data.Lens (_Left, _Right)
import Data.Map as Map
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T

data BuilderAction 
  = IncreaseAbilityScore Ability
  | DecreaseAbilityScore Ability
  | SetBackground Background
  | IncreaseSkill Skill
  | DecreaseSkill Skill

initialState :: CharacterBuilder
initialState = 
  { abilityPoints : 10
  , skillPoints  : 40
  , abilities : {strength : 2, comprehension : 2, intuition : 2, agility : 2}
  , skills : Map.empty
  , background : Nothing
  , race : Nothing
  }

spec :: T.Spec _ CharacterBuilder _ (Either AbilityAction RaceSelectorAction) 
spec = T.match _Left abilitySpec <> T.match _Right raceSelectorSpec

-- Renders the component by suppying the initial state
main = T.defaultMain spec initialState unit
