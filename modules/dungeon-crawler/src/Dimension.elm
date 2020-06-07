module Dimension exposing
    ( Dimension
    , containsLocation
    , new
    , toLocationRows
    , toLocations
    )

import Basics.More exposing (..)
import Location exposing (Location)


type Dimension
    = Dimension ( Int, Int )


new : Int -> Int -> Dimension
new w h =
    fromTuple ( w, h )


fromTuple : ( Int, Int ) -> Dimension
fromTuple =
    Dimension


toTuple : Dimension -> ( Int, Int )
toTuple (Dimension wh) =
    wh


toLocationRows : Dimension -> List (List Location)
toLocationRows d =
    let
        ( w, h ) =
            toTuple d
    in
    rangeLen2 h w
        |> List.map (List.map (\( y, x ) -> Location.new x y))


toLocations : Dimension -> List Location
toLocations =
    toLocationRows >> List.concat


containsLocation : Location -> Dimension -> Bool
containsLocation location d =
    let
        ( x, y ) =
            Location.toTuple location

        ( w, h ) =
            toTuple d
    in
    isValidIndex x w && isValidIndex y h
