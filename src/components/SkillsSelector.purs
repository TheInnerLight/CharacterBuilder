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
import Unsafe.Coerce
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
  | CreateSkill Skill String
  | DeleteSkill Skill String

performAction :: T.PerformAction _ CharacterBuilder _ SkillSelectorAction
performAction (IncreaseSkill skill maybeSubSkill) _ _ = void do
  T.modifyState $ transformSkill skill maybeSubSkill (\x -> x + 1)
performAction (DecreaseSkill skill maybeSubSkill) _ _ = void do
  T.modifyState $ transformSkill skill maybeSubSkill (\x -> x - 1)
performAction (CreateSkill skill subSkill) _ _ = void do
  T.modifyState (\state -> state { skills = SM.update skill (Just subSkill) (const 0) state.skills } )
performAction (DeleteSkill skill subSkill) _ _ = void do
  T.modifyState (\state -> state { skills = removeSkill skill (Just subSkill) state.skills } )

isIncreasable :: Skill -> Maybe String -> CharacterBuilder -> Boolean
isIncreasable skill maybeSubSkill cb = 
  (fromMaybe true $ map (\x -> x < 10) maybeValue) && increasableAfterChange
  where
  maybeValue = SM.getSkillValue skill maybeSubSkill (derivedSkills cb) -- skill maximums should be applied to derived skills
  increasableAfterChange = fromMaybe false $ map (\x -> x >= 0) skillPointsAfterChange
  skillPointsAfterChange = remainingSkillPoints $ transformSkill skill maybeSubSkill (\x -> x + 1) cb

isDecreasable :: Skill -> Maybe String -> CharacterBuilder -> Boolean
isDecreasable skill maybeSubSkill cb = 
  fromMaybe false $ map (\x -> x > 0) maybeValue
  where
  maybeValue = SM.getSkillValue skill maybeSubSkill cb.skills -- skill minimums should be applied to base skills

isDeletable :: Skill -> Maybe String -> CharacterBuilder -> Boolean
isDeletable skill maybeSubskill cb = 
  case cb.background of
    Just background -> isNothing $ SM.getSkillValue skill maybeSubskill background.startingSkills
    Nothing         -> true

elementFromSkill :: CharacterBuilder -> (SkillSelectorAction -> T.EventHandler) -> Tuple Skill (M.Map (Maybe String) Int) -> R.ReactElement
elementFromSkill cb dispatch (Tuple (Skill skill) subskills)  = 
  reactElem subskills
  where 
  singleReactElement (Tuple (Just subskill) v) = 
    [ R.p [ RP.className "Skill"]
          [ R.text   $ skill.name <> " (" <> subskill <> ")" <> ": "
          , R.text   $ show v
          , R.button [ RP.onClick \_ -> dispatch (IncreaseSkill wrappedSkill $ Just subskill)
                     , RP.disabled $ not $ isIncreasable wrappedSkill (Just subskill) cb ]
                     [ R.text "+" ]
          , R.button [ RP.onClick \_ -> dispatch (DecreaseSkill wrappedSkill $ Just subskill) 
                     , RP.disabled $ not $ isDecreasable wrappedSkill (Just subskill) cb ]
                     [ R.text "-" ]
          , R.button [ RP.onClick \_ -> dispatch (DeleteSkill wrappedSkill subskill) 
                     , RP.hidden $ not $ isDeletable wrappedSkill (Just subskill) cb ]
                     [ R.text "✖" ]
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
  reactElem subskills = R.p [ RP.className skill.name ] $
    case skill.skillType of
      FieldSpecific -> 
        A.concat [ bind (mapToArray subskills) singleReactElement
                 , [ R.p  [ RP.hidden $ not $ any (\x -> x > 0) (remainingSkillPoints cb) ]
                          [  R.text $ skill.name <> " ("
                          ,  R.input [ RP.onKeyUp \e -> handleKeyPress (unsafeCoerce e).keyCode (unsafeCoerce e).target.value]
                                    []
                          , R.text ")"
                          ]
                    ]
                 ]
      SingleValue -> bind (mapToArray subskills) singleReactElement
  handleKeyPress :: Int -> String -> _
  handleKeyPress 13 text = dispatch $ CreateSkill wrappedSkill $ (Str.toUpper $ Str.take 1 text) <> (Str.toLower $ Str.drop 1 text)
  handleKeyPress _  _    = pure unit

skillSelector :: T.Render CharacterBuilder _ _
skillSelector dispatch _ state _ =
  [ R.p' [R.text $ "Remaining Skill Points: " <> remainingSkillPointsText ] 
  ,  R.p [ RP.className "Skills"]
     (map (elementFromSkill state dispatch) skillArray)
  ]
  where 
  skillArray = A.sortBy (alphabeticalSkills) $ SM.mapToArray $ derivedSkills state
  alphabeticalSkills (Tuple (Skill s) _) (Tuple (Skill s') _) = Str.localeCompare s.name s'.name
  remainingSkillPointsText = case remainingSkillPoints state of
    Just x -> show x
    Nothing -> "----"

skillSelectorSpec :: T.Spec _ CharacterBuilder _ SkillSelectorAction
skillSelectorSpec = T.simpleSpec performAction skillSelector