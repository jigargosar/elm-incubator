module Dimension exposing
    ( Dimension
    , containsLocation
    , new
    , toLocationRows
    , toLocations
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


toLocationRows : Dimension -> List (List Location)
toLocationRows d =
    Index.range2 d.rows d.columns
        |> List.map (List.map Location.fromTuple)


toLocations : Dimension -> List Location
toLocations =
    toLocationRows >> List.concat


containsLocation : Location -> Dimension -> Bool
containsLocation location dimension =
    let
        ( row, column ) =
            Location.toTuple location
    in
    Index.memberOf dimension.rows row
        && Index.memberOf dimension.columns column
