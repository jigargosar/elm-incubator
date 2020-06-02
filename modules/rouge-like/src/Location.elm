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


type alias Location =
    { x : Int
    , y : Int
    }


new x y =
    Location x y


toTuple : Location -> ( Int, Int )
toTuple p =
    ( p.x, p.y )


map : (Int -> Int) -> Location -> Location
map f l =
    new (f l.x) (f l.y)


map2 : (Int -> Int -> Int) -> Location -> Location -> Location
map2 f a b =
    new (f a.x b.x) (f a.y b.y)


any : (Int -> Bool) -> Location -> Bool
any f x =
    f x.y || f x.x


fromTuple : ( Int, Int ) -> Location
fromTuple ( x, y ) =
    new x y


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
mapX f location =
    { location | x = f location.x }


mapY : (Int -> Int) -> Location -> Location
mapY f location =
    { location | y = f location.y }


manhattanDistance : Location -> Location -> Int
manhattanDistance a b =
    let
        ( x, y ) =
            toTuple a

        ( x2, y2 ) =
            toTuple b
    in
    abs (y2 - y) + abs (x2 - x)
