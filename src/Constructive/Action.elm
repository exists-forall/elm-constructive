module Constructive.Action
    ( Action (Replace, Other)
    , Viewer
    , Never
    , interceptOthers
    , interceptReplacements
    , mapReplacements
    , extractReplacementModel
    ) where


{-| A standard interface for representing and working with actions in
elm-constructive.  The basic idea is that an elm-constructive action is
generally just a simple wrapper around a complete replacement model, rather than
instructions for how to modify the model.

Actions represented in this way can take full advantage of elm-constructive's
view composition combinators.  Non-replacement actions can be represented within
this framework as well, but they will simply be "along for the ride," and will
not benefit from any combinators or composition features.

# Types and definitions

@docs Action, Viewer, Never

# Transforming actions

@docs interceptOthers, interceptReplacements, mapReplacements

# Working with `Never`

@docs extractReplacementModel
-}

{-| Action types which may consist of a new replacement `model`, or some
`other` action.

Use `Replace` actions to represent direct modification to the model:

    -- A simple counter
    viewCounter address count =
      div []
        [ button [ onClick address (Replace (count + 1)) ] [ text "+" ]
        , button [ onClick address (Replace (count - 1)) ] [ text "-" ]
        , text (toString count)
        ]

Sometimes, one needs to send an action which cannot be represented as a
replacement model.  You can represent these with the `Other` tag:

    -- A counter whose value can be shared on social media

    type Share = ShareOnFacebook Int | ShareOnTwitter Int

    viewShareableCounter : Signal.Address (Action Share Int) -> Int -> Html
    viewShareableCounter address count =
      div []
        [ button [ onClick address (Replace (count + 1)) ] [ text "+" ]
        , button [ onClick address (Replace (count - 1)) ] [ text "-" ]
        , text (toString count)
        , button
          [ onClick address (Other (ShareOnFacebook count)) ]
          [ text "Share your counter on facebook!" ]
        , button
          [ onClick address (Other (ShareOnTwitter count)) ]
          [ text "Share your counter on twitter!" ]
        ]

Action types which only make use of `Replace` should use `Never` as their
`other` type:

    viewToggleButton : Signal.Address (Action Never Bool) -> Bool -> Html
    viewToggleButton address flag =
      if flag
        then button [ onClick address (Replace False) ] [ text "ON" ]
        else button [ onClick address (Replace True) ] [ text "OFF" ]
-}
type Action other model
    = Replace model
    | Other other


{-| The basic unit of view logic in elm-constructive.  A view function takes a
`model`, produces a `view` for that model (usually `Html`), and wires up that
`view` to send `Actions` to a particular `Address`.

    viewCounter : Viewer Html Never Int

    viewSharableCounter : Viewer Html Share Int
    
    viewToggleButton : Viewer Html Never Bool
-}
type alias Viewer view other model =
    Signal.Address (Action other model) -> model -> view


{-| An "uninhabited" type with no values.  This is useful for representing that
an `Action` type consists only of `Replace` actions, and has no `Other` actions.

    viewToggleButton : Viewer Html Never Bool
-}
type Never = Never Never


{-| Flatmap a function over actions with the `Other` tag, leaving `Replace`
actions unchanged.

    interceptOthers (\ClearList -> Replace []) (Other ClearList) == Replace []

    interceptOthers (\ClearList -> Replace []) (Replace [1,2,3]) == Replace [1,2,3]

This is useful for having a child view produce an `Other` action which is then
transformed by the parent view into a `Replace` action for the parent model.

Partial applications of `interceptOthers` are designed to work well with
`Signal.forwardTo`
-}
interceptOthers : (other1 -> Action other2 model) -> Action other1 model -> Action other2 model
interceptOthers f action =
  case action of
    Other other -> f other
    Replace newModel -> Replace newModel


{-| Flatmap a function over actions with the `Replace` tag, leaving `Other`
actions unchanged.

    interceptReplacements (\newModel -> Replace (Just newModel)) (Replace "foo") == Replace (Just "foo")

    interceptReplacements (\newModel -> Replace (Just newModel)) (Other ClearAll) == Other ClearAll

Partial applications of `interceptReplacements` are designed to work well with
`Signal.forwardTo`

Although the transformation function can technically produce either an `Other`
action or a `Replace` action, in practice one usually wants it to return a
`Replace` action.  For most purposes, you should therefore use `mapReplacements`
instead, which is like `interceptReplacements`, except that it always transforms
`Replace` actions into `Replace` actions.
-}
interceptReplacements : (model1 -> Action other model2) -> Action other model1 -> Action other model2
interceptReplacements f action =
  case action of
    Other other -> Other other
    Replace newModel -> f newModel


{-| Map a function over the new models of `Replace` actions, leaving `Other`
actions unchanged.

    mapReplacements Just (Replace "foo") == Replace (Just "foo")

    mapReplacements Just (Other ClearAll) == Other ClearAll

Partial applications of `mapReplacements` are designed to work well with
`Signal.forwardTo`
-}
mapReplacements : (model1 -> model2) -> Action other model1 -> Action other model2
mapReplacements f action =
  case action of
    Other other -> Other other
    Replace newModel -> Replace (f newModel)


{-| Extract the replacement model from an `Action` which is guaranteed to be a
`Replace` action (i.e., from an `Action` where `other` is `Never`).

    extractReplcementModel (Replace "foo") == "foo"
-}
extractReplacementModel : Action Never model -> model
extractReplacementModel action =
  case action of
    Replace newModel -> newModel
    Other _ -> Debug.crash "If Elm's type system is sound, then it is impossible to construct a value of type `Never`.  Therefore, it should be impossible for this branch to be executed."
