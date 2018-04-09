module AbilitySelector where
  
import Abilities
import CharacterBuilder
import Data.Foldable
import Data.List
import Prelude
import Races
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T
import Type.Data.Boolean (kind Boolean)

data AbilityAction 
  = IncreaseAbilityScore Ability
  | DecreaseAbilityScore Ability

isIncreasable :: Ability -> CharacterBuilder -> Boolean
isIncreasable ability cb = abilityScore ability cb < 5

isDecreasable :: Ability -> CharacterBuilder -> Boolean
isDecreasable ability cb = abilityScore ability cb > 2

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
          , R.text   $ show derivedAbilities.strength
          , R.button [ RP.onClick \_ -> dispatch (IncreaseAbilityScore Strength)
                      , RP.disabled $ not $ isIncreasable Strength state ]
                    [ R.text "+" ]
          , R.button [ RP.onClick \_ -> dispatch (DecreaseAbilityScore Strength) 
                      , RP.disabled $ not $ isDecreasable Strength state ]
                    [ R.text "-" ]
          ]
      , R.p [ RP.className "Agility" ] 
          [ 
          R.text     $ "Agility: "
          , R.text   $ show derivedAbilities.agility
          , R.button [ RP.onClick \_ -> dispatch (IncreaseAbilityScore Agility)
                     , RP.disabled $ not $ isIncreasable Agility state ]
                    [ R.text "+" ]
          , R.button [ RP.onClick \_ -> dispatch (DecreaseAbilityScore Agility) 
                     , RP.disabled $ not $ isDecreasable Agility state ]
                    [ R.text "-" ]
          ]
      , R.p [ RP.className "Comprehension" ] 
          [ 
          R.text     $ "Comprehension: "
          , R.text   $ show derivedAbilities.comprehension
          , R.button [ RP.onClick \_ -> dispatch (IncreaseAbilityScore Comprehension) 
                     , RP.disabled $ not $ isIncreasable Comprehension state ]
                    [ R.text "+" ]
          , R.button [ RP.onClick \_ -> dispatch (DecreaseAbilityScore Comprehension) 
                     , RP.disabled $ not $ isDecreasable Comprehension state ]
                    [ R.text "-" ]
          ]
      , R.p [ RP.className "Intuition" ] 
          [ 
          R.text     $ "Intuition: "
          , R.text   $ show derivedAbilities.intuition
          , R.button [ RP.onClick \_ -> dispatch (IncreaseAbilityScore Intuition) 
                     , RP.disabled $ not $ isIncreasable Intuition state ]
                    [ R.text "+" ]
          , R.button [ RP.onClick \_ -> dispatch (DecreaseAbilityScore Intuition) 
                    , RP.disabled $ not $ isDecreasable Intuition state ]
                    [ R.text "-" ]
          ]
      , R.p [ RP.className "Health" ] 
          [ 
          R.text     $ "Health: "
          , R.text   $ show derivedAbilities.health
          ]
      , R.p [ RP.className "Resolve" ] 
          [ 
          R.text     $ "Resolve: "
          , R.text   $ show derivedAbilities.resolve
          ]    
    ]
  ]
  where 
  derivedAbilities = calculateDerivedAbilities state

abilitySpec :: T.Spec _ CharacterBuilder _ AbilityAction
abilitySpec = T.simpleSpec performAction render