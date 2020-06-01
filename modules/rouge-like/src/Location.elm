module Location exposing
    ( Location
    , adjacent
    , down
    , fromTuple
    , left
    , manhattanDistance
    , new
    , right
    , toTuple
    , up
    )


type alias Location =
    { row : Int
    , column : Int
    }


new =
    Location


toTuple : Location -> ( Int, Int )
toTuple p =
    ( p.row, p.column )


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
    { model | column = f model.column }


mapRow : (Int -> Int) -> Location -> Location
mapRow f model =
    { model | row = f model.row }


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
