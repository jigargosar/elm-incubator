module Position exposing (Position, adjacent, down, fromTuple, left, new, right, toTuple, up)


type alias Position =
    { row : Int
    , column : Int
    }


new =
    Position


toTuple : Position -> ( Int, Int )
toTuple p =
    ( p.row, p.column )


fromTuple : ( Int, Int ) -> Position
fromTuple ( r, c ) =
    new r c


adjacentFS : List (Position -> Position)
adjacentFS =
    [ up, right, down, left ]


adjacent : Position -> List Position
adjacent position =
    adjacentFS
        |> List.map (\f -> f position)


left : Position -> Position
left =
    mapColumn (add -1)


right : Position -> Position
right =
    mapColumn (add 1)


up : Position -> Position
up =
    mapRow (add -1)


down : Position -> Position
down =
    mapRow (add 1)


mapColumn : (Int -> Int) -> Position -> Position
mapColumn f model =
    { model | column = f model.column }


mapRow : (Int -> Int) -> Position -> Position
mapRow f model =
    { model | row = f model.row }


add =
    (+)
