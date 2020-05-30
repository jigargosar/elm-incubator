module RC exposing
    ( RC
    , adjacent
    , down
    , fromTuple
    , left
    , new
    , right
    , toTuple
    , up
    )


type alias RC =
    ( Int, Int )


new : Int -> Int -> RC
new =
    Tuple.pair


toTuple : RC -> ( Int, Int )
toTuple =
    identity


fromTuple : ( Int, Int ) -> RC
fromTuple =
    identity


adjacentFS : List (RC -> RC)
adjacentFS =
    [ up, right, down, left ]


adjacent : RC -> List RC
adjacent position =
    adjacentFS
        |> List.map (\f -> f position)


left : RC -> RC
left =
    mapColumn (add -1)


right : RC -> RC
right =
    mapColumn (add 1)


up : RC -> RC
up =
    mapRow (add -1)


down : RC -> RC
down =
    mapRow (add 1)


mapColumn : (Int -> Int) -> RC -> RC
mapColumn =
    Tuple.mapSecond


mapRow : (Int -> Int) -> RC -> RC
mapRow =
    Tuple.mapFirst


add =
    (+)
