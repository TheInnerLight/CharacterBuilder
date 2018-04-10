module SkillsSelector where 

import CharacterBuilder
import Data.Foldable
import Data.List
import Prelude
import SkillMap
import Skills

import Data.Tuple
import Data.Maybe
import Data.Array as A
import Data.Map as M
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import SkillMap as SM
import Thermite as T

data SkillSelectorAction
  = IncreaseSkill Skill
  | DecreaseSkill Skill

performAction :: T.PerformAction _ CharacterBuilder _ SkillSelectorAction
performAction _ _ _ = pure unit

elementFromSkill :: (SkillSelectorAction -> T.EventHandler) -> Tuple Skill (M.Map (Maybe String) Int) -> R.ReactElement
elementFromSkill dispatch (Tuple (Skill skill) map) = 
  reactElem $ map
  where 
  singleReactElement (Tuple (Just subskill) v) = 
    [ R.p [ RP.className "Skills"]
          [ R.text   $ skill.name <> "(" <> subskill <> ")" <> ": "
          , R.text   $ show v
          , R.button [ RP.onClick \_ -> dispatch (DecreaseSkill wrappedSkill)
                      ]
                      [ R.text "+" ]
          , R.button [ RP.onClick \_ -> dispatch (DecreaseSkill wrappedSkill) 
                      ]
                      [ R.text "-" ]
          ]
    ]
  singleReactElement (Tuple Nothing v) = 
    [ R.p [ RP.className "Skills"]
          [ R.text   $ skill.name <> ": "
          , R.text   $ show v
          , R.button [ RP.onClick \_ -> dispatch (DecreaseSkill wrappedSkill)
                      ]
                      [ R.text "+" ]
          , R.button [ RP.onClick \_ -> dispatch (DecreaseSkill wrappedSkill) 
                      ]
                      [ R.text "-" ]
          ]
    ]
  wrappedSkill = Skill skill  
  reactElem (map) = R.p [ RP.className skill.name ] (bind (mapToArray map) singleReactElement)

skillSelector :: T.Render CharacterBuilder _ _
skillSelector dispatch _ state _ =
  [ R.p [ RP.className "Skills"]
      (map (elementFromSkill dispatch) (SM.mapToArray $ derivedSkills state))
  ]

skillSelectorSpec :: T.Spec _ CharacterBuilder _ SkillSelectorAction
skillSelectorSpec = T.simpleSpec performAction skillSelector