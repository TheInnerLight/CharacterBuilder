module AbilitySelector where
  
import Prelude
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T
import Abilities
import CharacterBuilder

data AbilityAction 
  = IncreaseAbilityScore Ability
  | DecreaseAbilityScore Ability

performAction :: T.PerformAction _ CharacterBuilder _ AbilityAction
performAction (IncreaseAbilityScore Strength) _ _ = void do
  T.modifyState (\state -> state { abilities { strength = state.abilities.strength + 1} } )
performAction (DecreaseAbilityScore Strength) _ _ = void do
  T.modifyState (\state -> state { abilities { strength = state.abilities.strength - 1} } )
performAction (IncreaseAbilityScore Agility) _ _ = void do
  T.modifyState (\state -> state { abilities { agility = state.abilities.agility + 1} } )
performAction (DecreaseAbilityScore Agility) _ _ = void do
  T.modifyState (\state -> state { abilities { agility = state.abilities.agility - 1} } )
performAction (IncreaseAbilityScore Comprehension) _ _ = void do
  T.modifyState (\state -> state { abilities { comprehension = state.abilities.comprehension + 1} } )
performAction (DecreaseAbilityScore Comprehension) _ _ = void do
  T.modifyState (\state -> state { abilities { comprehension = state.abilities.comprehension - 1} } )
performAction (IncreaseAbilityScore Intuition) _ _ = void do
  T.modifyState (\state -> state { abilities { intuition = state.abilities.intuition + 1} } )
performAction (DecreaseAbilityScore Intuition) _ _ = void do
  T.modifyState (\state -> state { abilities { intuition = state.abilities.intuition - 1} } )
performAction _ _ _ = void do
  pure unit
  
render :: T.Render CharacterBuilder _ _
render dispatch _ state _ =
  [ R.p [ RP.className "Abilities"]
    [ R.text "Remaining Ability Points: "
      , R.text $ show $ remainingAbilityPoints state
      , R.p [ RP.className "Strength" ] 
          [ 
          R.text     $ "Strength: "
          , R.text   $ show state.abilities.strength
          , R.button [ RP.onClick \_ -> dispatch (IncreaseAbilityScore Strength) ]
                    [ R.text "+" ]
          , R.button [ RP.onClick \_ -> dispatch (DecreaseAbilityScore Strength) ]
                    [ R.text "-" ]
          ]
      , R.p [ RP.className "Agility" ] 
          [ 
          R.text     $ "Agility: "
          , R.text   $ show state.abilities.agility
          , R.button [ RP.onClick \_ -> dispatch (IncreaseAbilityScore Agility) ]
                    [ R.text "+" ]
          , R.button [ RP.onClick \_ -> dispatch (DecreaseAbilityScore Agility) ]
                    [ R.text "-" ]
          ]
      , R.p [ RP.className "Comprehension" ] 
          [ 
          R.text     $ "Comprehension: "
          , R.text   $ show state.abilities.comprehension
          , R.button [ RP.onClick \_ -> dispatch (IncreaseAbilityScore Comprehension) ]
                    [ R.text "+" ]
          , R.button [ RP.onClick \_ -> dispatch (DecreaseAbilityScore Comprehension) ]
                    [ R.text "-" ]
          ]
      , R.p [ RP.className "Intuition" ] 
          [ 
          R.text     $ "Intuition: "
          , R.text   $ show state.abilities.intuition
          , R.button [ RP.onClick \_ -> dispatch (IncreaseAbilityScore Intuition) ]
                    [ R.text "+" ]
          , R.button [ RP.onClick \_ -> dispatch (DecreaseAbilityScore Intuition) ]
                    [ R.text "-" ]
          ]
    ]
  ]

abilitySpec :: T.Spec _ CharacterBuilder _ AbilityAction
abilitySpec = T.simpleSpec performAction render