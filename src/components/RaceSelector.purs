module RaceSelector where

import Prelude
import Abilities
import Data.Foldable
import Data.Maybe
import Prelude
import Races
import CharacterBuilder (CharacterBuilder)
import React (ReactElement)
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T

data RaceSelectorAction
  = SelectRace Race


performAction :: T.PerformAction _ CharacterBuilder _ RaceSelectorAction
performAction (SelectRace race) _ _ = void do
  T.modifyState (\state -> state { race = Just race } )

optionElementFromRace :: (RaceSelectorAction -> T.EventHandler) -> Race -> ReactElement
optionElementFromRace dispatch race = 
  R.option [ RP.onClick \_ -> dispatch (SelectRace race)]
           [ R.text (race.name)]

raceSelector :: T.Render CharacterBuilder _ _
raceSelector dispatch _ state _ =
  [ R.p [ RP.className "Race"]
      [ R.text "Race: "
        , R.select [RP.className "RaceSelector"]
        (map (optionElementFromRace dispatch) races)
      ]
  ]

raceSelectorSpec :: T.Spec _ CharacterBuilder _ RaceSelectorAction
raceSelectorSpec = T.simpleSpec performAction raceSelector