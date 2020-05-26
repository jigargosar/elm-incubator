module Dimension exposing (Dimension, member, new, toPositions, toRows, toTuple)

import Index
import Position exposing (Position)


type alias Dimension =
    { rows : Int
    , columns : Int
    }


new =
    Dimension


toRows : Dimension -> List (List Position)
toRows d =
    Index.range d.rows
        |> List.map
            (\r ->
                Index.range d.columns
                    |> List.map (\c -> Position.new r c)
            )


toPositions : Dimension -> List Position
toPositions =
    toRows >> List.concat


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
