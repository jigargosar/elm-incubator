module Dimension exposing (Dimension, member, new, toRows)

import Index
import Position exposing (Position)


type alias Dimension =
    { rows : Int
    , columns : Int
    }


new =
    Dimension


toRows : { a | rows : Int, columns : Int } -> List (List Position)
toRows d =
    Index.range d.rows
        |> List.map
            (\r ->
                Index.range d.columns
                    |> List.map (\c -> Position.new r c)
            )


member : Position -> Dimension -> Bool
member position dimension =
    case Position.toTuple position of
        ( row, column ) ->
            Index.memberOf dimension.rows row
                && Index.memberOf dimension.columns column
