module Dimension exposing
    ( Dimension
    , containsPosition
    , maxPosition
    , new
    , toPositionRows
    , toPositions
    )

import Index
import Location exposing (Location)


type alias Dimension =
    { rows : Int
    , columns : Int
    }


new : Int -> Int -> Dimension
new =
    Dimension


toPositionRows : Dimension -> List (List Location)
toPositionRows d =
    Index.range2 d.rows d.columns
        |> List.map (List.map Location.fromTuple)


toPositions : Dimension -> List Location
toPositions =
    toPositionRows >> List.concat


containsPosition : Location -> Dimension -> Bool
containsPosition position dimension =
    let
        ( row, column ) =
            Location.toTuple position
    in
    Index.memberOf dimension.rows row
        && Index.memberOf dimension.columns column


maxPosition : Dimension -> Location
maxPosition dimension =
    Location.new (dimension.rows - 1) (dimension.columns - 1)
