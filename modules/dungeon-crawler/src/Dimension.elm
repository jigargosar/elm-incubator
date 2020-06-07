module Dimension exposing
    ( Dimension
    , isValidLocation
    , new
    , toFloatScaled
    , toLocationRows
    , toLocations
    , toTuple
    )

import Basics.More exposing (..)
import Location exposing (Location)
import Tuple.More as Tuple


type Dimension
    = Dimension ( Int, Int )


new : Int -> Int -> Dimension
new w h =
    int2Pair w h |> fromTuple


fromTuple : ( Int, Int ) -> Dimension
fromTuple =
    Dimension


toTuple : Dimension -> Int2
toTuple (Dimension wh) =
    wh


toFloatScaled : Float -> Dimension -> Float2
toFloatScaled n =
    toTuple >> Tuple.toFloatScaled n


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


isValidLocation : Location -> Dimension -> Bool
isValidLocation location d =
    let
        ( x, y ) =
            Location.toTuple location

        ( w, h ) =
            toTuple d
    in
    isValidIndex x w && isValidIndex y h
