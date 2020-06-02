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
        |> List.map (List.map (\( y, x ) -> Location.new x y))


toLocations : Dimension -> List Location
toLocations =
    toLocationRows >> List.concat


containsLocation : Location -> Dimension -> Bool
containsLocation location dimension =
    let
        ( x, y ) =
            Location.toTuple location
    in
    Index.memberOf dimension.rows y
        && Index.memberOf dimension.columns x
