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
    { width : Int
    , height : Int
    }


new : Int -> Int -> Dimension
new w h =
    Dimension w h


toLocationRows : Dimension -> List (List Location)
toLocationRows d =
    Index.range2 d.height d.width
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
    Index.memberOf dimension.width x
        && Index.memberOf dimension.height y
