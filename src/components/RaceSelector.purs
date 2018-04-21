module RaceSelector where

import Prelude
import Data.Maybe (Maybe(..))
import Races (Race, races)
import CharacterBuilder (CharacterBuilder)
import Data.Array as A
import React (ReactElement)
import React.DOM as R
import React.DOM.Props as RP
import Thermite as T
import Unsafe.Coerce (unsafeCoerce)

data RaceSelectorAction
  = SelectRace (Maybe Race)

raceToSelectionString :: Race -> String
raceToSelectionString race = race.name

selectionStringToRace :: String -> Maybe Race
selectionStringToRace race = A.find (\r -> r.name == race) races

-- Handles updating the state of a character builder based on a race selector action.
performAction :: T.PerformAction _ CharacterBuilder _ RaceSelectorAction
performAction (SelectRace maybeRace) _ _ = void do
  T.modifyState (\state -> state { race = maybeRace } )

-- Creates a a react option element from a supplied race and race selector action event handler.
optionElementFromRace :: (RaceSelectorAction -> T.EventHandler) -> Race -> ReactElement
optionElementFromRace dispatch race = 
  R.option [ RP.value $ raceToSelectionString race]
           [ R.text (race.name)]

-- A renderer for a component that allows you to select a race from the list of races.
raceSelector :: T.Render CharacterBuilder _ _
raceSelector dispatch _ state _ =
  [ R.div [ RP.className "character-builder-component"]
    [ R.div [ RP.className "title-and-selector"]
      [ R.text "Race: "
      , R.select [ RP.className "race-selector"
                  , RP.onChange (\e -> dispatch $ SelectRace $ selectionStringToRace (unsafeCoerce e).target.value) ]
      (A.concat
        [ [ R.option [ RP.value "Nothing"]
                     [ R.text ("----")]
          ]
        , map (optionElementFromRace dispatch) races
        ])
      ]
      , R.div' 
          case state.race of
            Just race -> A.concat 
              [ map (\x -> R.p' [R.text $ show x]) race.advantages
              , map (\x -> R.p' [R.text $ show x]) race.disadvantages
              , [ R.p' [R.text $ "Fate: " <> (show race.fate)]
                ]
              ]
            Nothing -> []
      ]
  ]

raceSelectorSpec :: T.Spec _ CharacterBuilder _ RaceSelectorAction
raceSelectorSpec = T.simpleSpec performAction raceSelector