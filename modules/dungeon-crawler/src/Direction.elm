module Direction exposing
    ( Direction(..)
    , directionFromArrowKey
    , stepLocationInDirection
    )

import Location exposing (Location)


stepLocationInDirection : Direction -> Location -> Location
stepLocationInDirection direction =
    case direction of
        Left ->
            Location.left

        Right ->
            Location.right

        Up ->
            Location.up

        Down ->
            Location.down


directionFromArrowKey : String -> Maybe Direction
directionFromArrowKey key =
    case key of
        "ArrowLeft" ->
            Just Left

        "ArrowRight" ->
            Just Right

        "ArrowUp" ->
            Just Up

        "ArrowDown" ->
            Just Down

        _ ->
            Nothing


type Direction
    = Left
    | Right
    | Up
    | Down
