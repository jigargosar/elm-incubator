module Dimension exposing
    ( Dimension
    , containsPosition
    , maxPosition
    , new
    , toPositionRows
    , toPositions
    )

import Index
import Position exposing (Position)


type alias Dimension =
    { rows : Int
    , columns : Int
    }


new : Int -> Int -> Dimension
new =
    Dimension


toPositionRows : Dimension -> List (List Position)
toPositionRows d =
    Index.range2 d.rows d.columns
        |> List.map (List.map Position.fromTuple)


toPositions : Dimension -> List Position
toPositions =
    toPositionRows >> List.concat


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
