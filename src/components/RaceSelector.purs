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

applyRacialStatBonus :: Race -> PrimaryAbilities -> PrimaryAbilities
applyRacialStatBonus race pa =
  foldl folder pa race.advantages
  where 
  folder acc (AbilityBonus Strength v) = acc {strength = acc.strength + v}
  folder acc (AbilityBonus Agility v) = acc {agility = acc.agility + v}
  folder acc (AbilityBonus Intuition v) = acc {intuition = acc.intuition + v}
  folder acc (AbilityBonus Comprehension v) = acc {comprehension = acc.comprehension + v}
  folder acc _ = acc

unapplyRacialStatBonus :: Race -> PrimaryAbilities -> PrimaryAbilities
unapplyRacialStatBonus race pa =
  foldl folder pa race.advantages
  where 
  folder acc (AbilityBonus Strength v) = acc {strength = acc.strength - v}
  folder acc (AbilityBonus Agility v) = acc {agility = acc.agility - v}
  folder acc (AbilityBonus Intuition v) = acc {intuition = acc.intuition - v}
  folder acc (AbilityBonus Comprehension v) = acc {comprehension = acc.comprehension - v}
  folder acc _ = acc

test :: Maybe Race -> PrimaryAbilities -> PrimaryAbilities
test mrace pa = foldl (\acc race -> unapplyRacialStatBonus race pa) pa mrace

performAction :: T.PerformAction _ CharacterBuilder _ RaceSelectorAction
performAction (SelectRace race) _ _ = void do
  T.modifyState (\state -> state 
    { race = Just race
    , abilities = applyRacialStatBonus race $ foldl (\acc race -> unapplyRacialStatBonus race acc) state.abilities state.race
    } )

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