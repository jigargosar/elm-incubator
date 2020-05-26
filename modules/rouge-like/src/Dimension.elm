module Dimension exposing (Dimension, member, new, toRows, toTuple)

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


toTuple : Dimension -> ( Int, Int )
toTuple dimension =
    ( dimension.rows, dimension.columns )


member : Position -> Dimension -> Bool
member position dimension =
    let
        ( row, column ) =
            Position.toTuple position
    in
    Index.memberOf dimension.rows row
        && Index.memberOf dimension.columns column
