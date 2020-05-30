module Dimension exposing
    ( Dimension
    , containsPosition
    , containsRC
    , maxPosition
    , maxRC
    , new
    , toPositionRows
    , toPositions
    , toRCRows
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


toPositionRows : Dimension -> List (List Position)
toPositionRows d =
    Index.range2 d.rows d.columns
        |> List.map (List.map Position.fromTuple)


toPositions : Dimension -> List Position
toPositions =
    toPositionRows >> List.concat


containsRC : RC -> Dimension -> Bool
containsRC ( r, c ) d =
    (r < 0 || c < 0 || r >= d.rows || c >= d.columns)
        |> not


toRCRows : Dimension -> List (List RC)
toRCRows d =
    Index.range2 d.rows d.columns


maxRC : Dimension -> RC
maxRC d =
    RC.new (d.rows - 1) (d.columns - 1)


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
