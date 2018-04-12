module BackgroundSelector where

import Data.Array as A
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
  = SelectBackground (Maybe Background)

performAction :: T.PerformAction _ CharacterBuilder _ BackgroundSelectorAction
performAction (SelectBackground maybeBackground) _ _ = void do
  T.modifyState (\state -> state { background = maybeBackground} )

optionElementFromBackground :: (BackgroundSelectorAction -> T.EventHandler) -> Background -> ReactElement
optionElementFromBackground dispatch background = 
  R.option [ RP.onClick \_ -> dispatch (SelectBackground $ Just background)]
           [ R.text (background.name)]

backgroundSelector :: T.Render CharacterBuilder _ _
backgroundSelector dispatch _ state _ =
  [ R.p [ RP.className "Background"]
    [ R.text "Background: "
      , R.select [RP.className "BackgroundSelector"]
      (A.concat [
          [ R.option [ RP.onClick \_ -> dispatch (SelectBackground $ Nothing)]
                    [ R.text ("----")]
          ]
          , map (optionElementFromBackground dispatch) backgrounds]
      )
      , R.p' 
          case state.background of
            Just background -> A.concat 
              [ map (\x -> R.p' [R.text $ show x]) background.freeSkillBonuses
              ]
            Nothing -> []

    ]
  ]

backgroundSelectorSpec :: T.Spec _ CharacterBuilder _ BackgroundSelectorAction
backgroundSelectorSpec = T.simpleSpec performAction backgroundSelector