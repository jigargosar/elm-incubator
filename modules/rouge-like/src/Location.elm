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


type alias Location =
    { y : Int
    , x : Int
    }


new =
    Location


toTuple : Location -> ( Int, Int )
toTuple p =
    ( p.y, p.x )


map : (Int -> Int) -> Location -> Location
map f l =
    new (f l.y) (f l.x)


map2 : (Int -> Int -> Int) -> Location -> Location -> Location
map2 f a b =
    new (f a.y b.y) (f a.x b.x)


any : (Int -> Bool) -> Location -> Bool
any f x =
    f x.y || f x.x


fromTuple : ( Int, Int ) -> Location
fromTuple ( r, c ) =
    new r c


adjacentFS : List (Location -> Location)
adjacentFS =
    [ up, right, down, left ]


adjacent : Location -> List Location
adjacent position =
    adjacentFS
        |> List.map (\f -> f position)


left : Location -> Location
left =
    mapColumn (add -1)


right : Location -> Location
right =
    mapColumn (add 1)


up : Location -> Location
up =
    mapRow (add -1)


down : Location -> Location
down =
    mapRow (add 1)


mapColumn : (Int -> Int) -> Location -> Location
mapColumn f model =
    { model | x = f model.x }


mapRow : (Int -> Int) -> Location -> Location
mapRow f model =
    { model | y = f model.y }


add =
    (+)


manhattanDistance : Location -> Location -> Int
manhattanDistance a b =
    let
        ( r, c ) =
            toTuple a

        ( r2, c2 ) =
            toTuple b
    in
    abs (r2 - r) + abs (c2 - c)
