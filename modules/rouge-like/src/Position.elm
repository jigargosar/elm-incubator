module Position exposing (Position, new)


type alias Position =
    { row : Int
    , column : Int
    }


new =
    Position
