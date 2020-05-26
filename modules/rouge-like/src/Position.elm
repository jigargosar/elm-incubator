module Position exposing (Position, fromTuple, new, toTuple)


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
