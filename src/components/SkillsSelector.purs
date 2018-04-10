module SkillsSelector where 

import CharacterBuilder
import Control.Monad
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Tuple
import Prelude
import SkillMap
import Skills

import Data.Array as A
import Data.Map as M
import Data.String as Str
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import SkillMap as SM
import Thermite as T

data SkillSelectorAction
  = IncreaseSkill Skill (Maybe String)
  | DecreaseSkill Skill (Maybe String)

performAction :: T.PerformAction _ CharacterBuilder _ SkillSelectorAction
performAction (IncreaseSkill skill maybeSubSkill) _ _ = void do
  T.modifyState (\state -> state { skills = SM.update skill maybeSubSkill ((+) 1) state.skills } )
performAction (DecreaseSkill skill maybeSubSkill) _ _ = void do
  T.modifyState (\state -> state { skills = SM.update skill maybeSubSkill (\x -> x - 1) state.skills } )


isIncreasable :: Skill -> Maybe String -> CharacterBuilder -> Boolean
isIncreasable skill maybeSubSkill cb = 
  fromMaybe true $ map (\x -> x < 10) maybeValue
  where
  maybeValue = SM.getSkillValue skill maybeSubSkill (derivedSkills cb) -- skill maximums should be applied to derived skills

isDecreasable :: Skill -> Maybe String -> CharacterBuilder -> Boolean
isDecreasable skill maybeSubSkill cb = 
  fromMaybe false $ map (\x -> x > 0) maybeValue
  where
  maybeValue = SM.getSkillValue skill maybeSubSkill cb.skills -- skill minimums should be applied to base skills

elementFromSkill :: CharacterBuilder -> (SkillSelectorAction -> T.EventHandler) -> Tuple Skill (M.Map (Maybe String) Int) -> R.ReactElement
elementFromSkill cb dispatch (Tuple (Skill skill) map)  = 
  reactElem $ map
  where 
  singleReactElement (Tuple (Just subskill) v) = 
    [ R.p [ RP.className "Skill"]
          [ R.text   $ skill.name <> "(" <> subskill <> ")" <> ": "
          , R.text   $ show v
          , R.button [ RP.onClick \_ -> dispatch (IncreaseSkill wrappedSkill $ Just subskill)
                     , RP.disabled $ not $ isIncreasable wrappedSkill (Just subskill) cb ]
                     [ R.text "+" ]
          , R.button [ RP.onClick \_ -> dispatch (DecreaseSkill wrappedSkill $ Just subskill) 
                     , RP.disabled $ not $ isDecreasable wrappedSkill (Just subskill) cb ]
                     [ R.text "-" ]
          ]
    ]
  singleReactElement (Tuple Nothing v) = 
    [ R.p [ RP.className "Skill"]
          [ R.text   $ skill.name <> ": "
          , R.text   $ show v
          , R.button [ RP.onClick \_ -> dispatch (IncreaseSkill wrappedSkill Nothing)
                     , RP.disabled $ not $ isIncreasable wrappedSkill Nothing cb ]
                     [ R.text "+" ]
          , R.button [ RP.onClick \_ -> dispatch (DecreaseSkill wrappedSkill Nothing) 
                     , RP.disabled $ not $ isDecreasable wrappedSkill Nothing cb ]
                     [ R.text "-" ]
          ]
    ]
  wrappedSkill = Skill skill  
  reactElem (map) = R.p [ RP.className skill.name ] (bind (mapToArray map) singleReactElement)

skillSelector :: T.Render CharacterBuilder _ _
skillSelector dispatch _ state _ =
  [ R.p [ RP.className "Skills"]
      (A.concat 
      [ [R.text $ "Remaining Skill Points: " <> show (remainingSkillPoints state) ]
        , map (elementFromSkill state dispatch) skillArray])
               
  ]
  where 
  skillArray = A.sortBy (alphabeticalSkills) $ SM.mapToArray $ derivedSkills state
  alphabeticalSkills (Tuple (Skill s) _) (Tuple (Skill s') _) = Str.localeCompare s.name s'.name

skillSelectorSpec :: T.Spec _ CharacterBuilder _ SkillSelectorAction
skillSelectorSpec = T.simpleSpec performAction skillSelector