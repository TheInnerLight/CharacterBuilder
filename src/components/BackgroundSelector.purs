module BackgroundSelector where

import Background
import Data.Maybe
import Prelude
import CharacterBuilder (CharacterBuilder)
import Data.Array as A
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T
import Unsafe.Coerce (unsafeCoerce)

data BackgroundSelectorAction
  = SelectBackground (Maybe Background)

backgroundToSelectionString :: Background -> String
backgroundToSelectionString background = background.name

selectionStringToBackground :: String -> Maybe Background
selectionStringToBackground backgroundStr = A.find (\bg -> bg.name == backgroundStr) backgrounds

-- Handles updating the state of a character builder based on a background selector action.
performAction :: T.PerformAction _ CharacterBuilder _ BackgroundSelectorAction
performAction (SelectBackground maybeBackground) _ _ = void do
  T.modifyState (\state -> state { background = maybeBackground} )

-- Creates a a react option element from a supplied background and background selector action event handler.
optionElementFromBackground :: (BackgroundSelectorAction -> T.EventHandler) -> Background -> R.ReactElement
optionElementFromBackground dispatch background = 
  R.option [ RP.value $ backgroundToSelectionString background]
           [ R.text (background.name)]

-- A renderer for a component that allows you to select a background from the list of backgrounds.
backgroundSelector :: T.Render CharacterBuilder _ _
backgroundSelector dispatch _ state _ =
  [ R.div [ RP.className "character-builder-component"]
    [ R.div [ RP.className "title-and-selector"] 
      [ R.text "Background: "
      , R.select  [RP.className "background-selector"
                  , RP.onChange (\e -> dispatch $ SelectBackground $ selectionStringToBackground (unsafeCoerce e).target.value) ]
                  ( A.concat 
                    [[ R.option  [ RP.value "Nothing"] [ R.text ("----")]]
                    , map (optionElementFromBackground dispatch) backgrounds]
                  )
      ]
      , R.div'
          case state.background of
            Just background -> A.concat 
              [ map (\x -> R.p' [R.text $ show x]) background.freeSkillBonuses
              ]
            Nothing -> []

    ]
  ]

backgroundSelectorSpec :: T.Spec _ CharacterBuilder _ BackgroundSelectorAction
backgroundSelectorSpec = T.simpleSpec performAction backgroundSelector