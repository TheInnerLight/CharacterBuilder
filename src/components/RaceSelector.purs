module RaceSelector where

import Prelude
import Abilities
import Data.Array as A
import Data.Maybe
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
  = SelectRace (Maybe Race)


performAction :: T.PerformAction _ CharacterBuilder _ RaceSelectorAction
performAction (SelectRace maybeRace) _ _ = void do
  T.modifyState (\state -> state { race = maybeRace } )

optionElementFromRace :: (RaceSelectorAction -> T.EventHandler) -> Race -> ReactElement
optionElementFromRace dispatch race = 
  R.option [ RP.onClick \_ -> dispatch (SelectRace $ Just race)]
           [ R.text (race.name)]

raceSelector :: T.Render CharacterBuilder _ _
raceSelector dispatch _ state _ =
  [ R.p [ RP.className "Race"]
      [ R.text "Race: "
        , R.select [RP.className "RaceSelector"]
        (A.concat [
            [ R.option [ RP.onClick \_ -> dispatch (SelectRace $ Nothing)]
                     [ R.text ("----")]
            ]
            , map (optionElementFromRace dispatch) races]
        )
      , R.p' 
          case state.race of
            Just race -> A.concat 
              [ map (\x -> R.p' [R.text $ show x]) race.advantages
              , map (\x -> R.p' [R.text $ show x]) race.disadvantages
              ]
            Nothing -> []
      ]
  ]

raceSelectorSpec :: T.Spec _ CharacterBuilder _ RaceSelectorAction
raceSelectorSpec = T.simpleSpec performAction raceSelector