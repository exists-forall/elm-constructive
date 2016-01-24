module Constructive.Combinators
    ( viewFocus
    , viewMaybe
    , viewList
    , viewArray
    ) where

{-| 
# Composing Viewers

@docs viewFocus, viewList, viewMaybe, viewArray
-}


import Array exposing (Array)


import Focus exposing (Focus)


import Constructive.Action as Action exposing (Action, Viewer)


{-| Transform a viewer for a child into a viewer for a parent, given a
[`Focus`](http://package.elm-lang.org/packages/evancz/focus/latest/) from the
parent to the child.  Replacements of the child model are automatically
reflected as replacements of the parent model:

    type CounterPair = CounterPair Int Int

    viewCounterPair : Viewer Html Never CounterPair
    viewCounterPair address pair =
      div []
        [ focusView left viewCounter address pair
        , focusView right viewCounter address pair
        ]

    -- Focus boilerplate

    left : Focus CounterPair Int
    left = Focus.create
        (\CounterPair l _ -> l)
        (\f (CounterPair l r) -> (f l, r))

    right : Focus CounterPair Int
    right = Focus.create
        (\CounterPair _ r -> r)
        (\f (CounterPair l r) -> (l, f r))
-}
viewFocus : Focus big small -> Viewer view other small -> Viewer view other big
viewFocus focus viewSmall address big =
    let annotate = Action.mapReplacements (flip (Focus.set focus) big)
    in viewSmall (Signal.forwardTo address annotate) (Focus.get focus big)


type alias ListTuple a = Maybe (a, List a)


asTuple : Focus (List a) (Maybe (a, List a))
asTuple =
  let
    get list =
      case list of
        [] -> Nothing
        (x :: xs) -> Just (x, xs)

    update f list =
      case f (get list) of
        Nothing -> []
        Just (x, xs) -> x :: xs
  in
    Focus.create get update


fstFocus : Focus (a, b) a
fstFocus =
    Focus.create (\(x, y) -> x) (\f (x, y) -> (f x, y))


sndFocus : Focus (a, b) b
sndFocus =
    Focus.create (\(x, y) -> y) (\f (x, y) -> (x, f y))


{-| Transform a viewer for a `model` into a viewer for a `Maybe model`,
automatically wrapping replacement values in `Just`

Because it produces a `Maybe view`, this function should generally be used
in conjunction with `Maybe.withDefault`:

    viewMaybe viewStreetMap address currentLocation
      |> Maybe.withDefault (text "Location data not available")
-}
viewMaybe : Viewer view other model -> Viewer (Maybe view) other (Maybe model)
viewMaybe view address maybe =
    let annotate = Action.mapReplacements Just
    in Maybe.map (view (Signal.forwardTo address annotate)) maybe


viewListTuple
    :  Viewer view other model
    -> Viewer (List view) other (ListTuple model)
viewListTuple viewItem address =
  viewMaybe
    (\address tuple -> 
        viewFocus fstFocus viewItem address tuple
        :: viewFocus sndFocus (viewList viewItem) address tuple)
    address
    >> Maybe.withDefault []


{-| Transform a viewer for a `model` into a viewer for `List` of `model`s,
with updates to invidual list items automatically reflected in the entire list.

Because it produces a `List view`, it often makes sense to use the result of
this function as the body of a `div` or `span` when `view` is `Html`.

    viewCounters : Viewer Html Never (List Int)
    viewCounters address counters =
        div [] (viewList viewCounter address counters)
-}
viewList : Viewer view other model -> Viewer (List view) other (List model)
viewList viewItem =
    viewFocus asTuple (viewListTuple viewItem)


{-| Transform a viewer for a `model` into a viewer for an `Array` of `model`s,
with updates to individual array items automatically reflected in the entire
array.

Because it produces an `Array view`, it often makes sense to transform the
result of this function into a list, and use it as the body of a `div` or `span`
when `view` is `Html`.

    viewCounterArray : Viewer Html Never (Array Int)
    viewCounterArray address counters =
        div [] (Array.toList (viewArray viewCounter address counters))
-}
viewArray : Viewer view other model -> Viewer (Array view) other (Array model)
viewArray viewItem address items =
    let annotate i = Action.mapReplacements (flip (Array.set i) items)
    in
      Array.indexedMap
        (\i item -> viewItem (Signal.forwardTo address (annotate i)) item)
        items
