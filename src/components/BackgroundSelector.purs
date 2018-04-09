module BackgroundSelector where

import Data.Maybe
import Prelude

import Background 
import CharacterBuilder (CharacterBuilder)
import React (ReactElement)
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T

data BackgroundSelectorAction
  = SelectBackground Background

performAction :: T.PerformAction _ CharacterBuilder _ BackgroundSelectorAction
performAction (SelectBackground background) _ _ = void do
  T.modifyState (\state -> state { background = Just background} )

optionElementFromBackground :: (BackgroundSelectorAction -> T.EventHandler) -> Background -> ReactElement
optionElementFromBackground dispatch background = 
  R.option [ RP.onClick \_ -> dispatch (SelectBackground background)]
           [ R.text (background.name)]

backgroundSelector :: T.Render CharacterBuilder _ _
backgroundSelector dispatch _ state _ =
  [ R.p [ RP.className "Background"]
      [ R.text "Background: "
        , R.select [RP.className "BackgroundSelector"]
        (map (optionElementFromBackground dispatch) backgrounds)
      ]
  ]

backgroundSelectorSpec :: T.Spec _ CharacterBuilder _ BackgroundSelectorAction
backgroundSelectorSpec = T.simpleSpec performAction backgroundSelector