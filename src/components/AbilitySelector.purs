module AbilitySelector where
  
import Abilities
import CharacterBuilder
import Data.Maybe
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
isIncreasable ability cb = (abilityScore ability cb.abilities < 5 ) && increasableAfterChange
  where
  increasableAfterChange = fromMaybe false $ map (\x -> x >= 0) abilityPointsAfterChange
  abilityPointsAfterChange = remainingAbilityPoints $ transformAbility ability (\x -> x + 1) cb

isDecreasable :: Ability -> CharacterBuilder -> Boolean
isDecreasable ability cb = abilityScore ability cb.abilities > 2

performAction :: T.PerformAction _ CharacterBuilder _ AbilityAction
performAction (IncreaseAbilityScore ability) _ _ = void do
  T.modifyState $ transformAbility ability (\x -> x + 1)
performAction (DecreaseAbilityScore ability) _ _ = void do
  T.modifyState $ transformAbility ability (\x -> x - 1)
  
render :: T.Render CharacterBuilder _ _
render dispatch _ state _ =
  [ R.p [ RP.className "Abilities"]
    [ R.p' [ R.text "Remaining Ability Points: ", R.text remainingAbilityPointText ]
      , R.p [ RP.className "ability-score" ] 
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
      , R.p [ RP.className "ability-score" ] 
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
      , R.p [ RP.className "ability-score" ] 
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
      , R.p [ RP.className "ability-score" ] 
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
      , R.p [ RP.className "ability-score" ] 
          [ 
          R.text     $ "Health: "
          , R.text   $ show derivedAbilities.health
          ]
      , R.p [ RP.className "ability-score" ] 
          [ 
          R.text     $ "Resolve: "
          , R.text   $ show derivedAbilities.resolve
          ]    
    ]
  ]
  where 
  remainingAbilityPointText = case remainingAbilityPoints state of
    Just x -> show x
    Nothing -> "----"
  derivedAbilities = calculateDerivedAbilities state

abilitySpec :: T.Spec _ CharacterBuilder _ AbilityAction
abilitySpec = T.simpleSpec performAction render