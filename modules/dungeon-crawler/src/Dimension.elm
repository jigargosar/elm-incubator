module Dimension exposing
    ( Dimension
    , height
    , heightScaled
    , isValidLocation
    , new
    , toFloat
    , toFloatScaled
    , toLocationRows
    , toLocations
    , toTuple
    , width
    , widthScaled
    )

import Basics.More as B exposing (..)
import Location exposing (Location)
import Tuple exposing (first)
import Tuple.More as Tuple


type Dimension
    = Dimension ( Int, Int )


new : Int -> Int -> Dimension
new w h =
    pairInt w h |> fromTuple


fromTuple : ( Int, Int ) -> Dimension
fromTuple =
    Dimension


toTuple : Dimension -> Int2
toTuple (Dimension wh) =
    wh


toFloatScaled : Float -> Dimension -> Float2
toFloatScaled n =
    toTuple >> Tuple.toFloatScaled n


toFloat : Dimension -> Float2
toFloat =
    toTuple >> Tuple.toFloat


widthScaled : Float -> Dimension -> Float
widthScaled n =
    width >> B.toFloatScaled n


heightScaled : Float -> Dimension -> Float
heightScaled n =
    height >> B.toFloatScaled n


width : Dimension -> Int
width =
    toTuple >> first


height : Dimension -> Int
height =
    toTuple >> first


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
