module Main where

import Abilities
import AbilitySelector
import BackgroundSelector
import CharacterBuilder
import Data.Either
import Data.List
import Data.Maybe
import Data.Tuple
import Prelude
import RaceSelector
import Races
import Skills
import SkillsSelector

import Background (Background)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM) as DOM
import Data.Lens (_Left, _Right)
import Data.Map as M
import Data.Set as S
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import SkillMap as SM
import Thermite as T

data BuilderAction 
  = IncreaseAbilityScore Ability
  | DecreaseAbilityScore Ability
  | SetBackground Background
  | IncreaseSkill Skill
  | DecreaseSkill Skill

initialSkills :: SM.SkillMap
initialSkills =
  M.fromFoldable $ map (\(Skill skill) -> createSkill skill.skillType (Skill skill)) skills
  where
  createSkill SingleValue skill = SM.singleSkill skill 0
  createSkill FieldSpecific skill = SM.relatedSkills skill M.empty

initialState :: CharacterBuilder
initialState = 
  { abilityPoints : 10
  , skillPoints  : 52
  , abilities : {strength : 2, comprehension : 2, intuition : 2, agility : 2}
  , skills : initialSkills
  , background : Nothing
  , race : Nothing
  }

type CombinedAction = Either AbilityAction RaceSelectorAction

specC1 :: T.Spec _ CharacterBuilder _ CombinedAction
specC1 = T.match _Left abilitySpec <> T.match _Right raceSelectorSpec

type CombinedAction2 = Either CombinedAction BackgroundSelectorAction

specC2 :: T.Spec _ CharacterBuilder _ CombinedAction2
specC2 = T.match _Left specC1 <> T.match _Right backgroundSelectorSpec

spec :: T.Spec _ CharacterBuilder _ (Either CombinedAction2 SkillSelectorAction)
spec = T.match _Left specC2 <> T.match _Right skillSelectorSpec

-- Renders the component by suppying the initial state
main = T.defaultMain spec initialState unit
