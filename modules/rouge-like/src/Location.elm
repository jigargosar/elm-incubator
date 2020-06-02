module Location exposing
    ( Location
    , adjacent
    , any
    , down
    , fromTuple
    , left
    , manhattanDistance
    , map
    , map2
    , new
    , right
    , toTuple
    , up
    )

import Basics.More exposing (..)
import Tuple.More as Tuple


type Location
    = Loc Int2


new : Int -> Int -> Location
new =
    curry fromTuple


fromTuple : ( Int, Int ) -> Location
fromTuple =
    Loc


toTuple : Location -> Int2
toTuple (Loc p) =
    p


map : (Int -> Int) -> Location -> Location
map f =
    toTuple >> Tuple.map f >> fromTuple


map2 : (Int -> Int -> Int) -> Location -> Location -> Location
map2 f a b =
    Tuple.map2 f (toTuple a) (toTuple b)
        |> fromTuple


any : (Int -> Bool) -> Location -> Bool
any f =
    toTuple >> Tuple.any f


adjacentFS : List (Location -> Location)
adjacentFS =
    [ up, right, down, left ]


adjacent : Location -> List Location
adjacent location =
    adjacentFS
        |> List.map (\f -> f location)


left : Location -> Location
left =
    mapX (add -1)


right : Location -> Location
right =
    mapX (add 1)


up : Location -> Location
up =
    mapY (add -1)


down : Location -> Location
down =
    mapY (add 1)


mapX : (Int -> Int) -> Location -> Location
mapX f =
    toTuple >> Tuple.mapFirst f >> fromTuple


mapY : (Int -> Int) -> Location -> Location
mapY f =
    toTuple >> Tuple.mapSecond f >> fromTuple


manhattanDistance : Location -> Location -> Int
manhattanDistance a b =
    let
        ( x, y ) =
            toTuple a

        ( x2, y2 ) =
            toTuple b
    in
    abs (y2 - y) + abs (x2 - x)
