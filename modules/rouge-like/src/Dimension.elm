module Dimension exposing
    ( Dimension
    , containsPosition
    , maxPosition
    , new
    , toPositions
    , toRows
    )

import Index
import Position exposing (Position)
import RC exposing (RC)


type alias Dimension =
    { rows : Int
    , columns : Int
    }


new : Int -> Int -> Dimension
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


containsRC : RC -> Dimension -> Bool
containsRC ( r, c ) d =
    (r < 0 || c < 0 || r >= d.rows || c >= d.columns)
        |> not


containsPosition : Position -> Dimension -> Bool
containsPosition position dimension =
    let
        ( row, column ) =
            Position.toTuple position
    in
    Index.memberOf dimension.rows row
        && Index.memberOf dimension.columns column


maxPosition : Dimension -> Position
maxPosition dimension =
    Position.new (dimension.rows - 1) (dimension.columns - 1)
