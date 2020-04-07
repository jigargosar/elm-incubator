module Main2 exposing (main)

import Basics.Extra exposing (uncurry)
import Playground exposing (..)



-- Game Scaffold


type Mem
    = Mem


init : Mem
init =
    Mem


gridXLength =
    10


gridYLength =
    10


gridCellWidth =
    50


gridPositions : List ( Int, Int )
gridPositions =
    List.range 0 (gridXLength - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (gridYLength - 1) |> List.map (Tuple.pair x)
            )


gIdxToScreen : ( Int, Int ) -> ( Float, Float )
gIdxToScreen ( x, y ) =
    ( toFloat x * gridCellWidth, toFloat y * 50 )


update : Computer -> Mem -> Mem
update computer mem =
    mem


view : Computer -> Mem -> List Shape
view computer mem =
    [ group (List.map renderCell gridPositions)
    ]


renderCell : ( Int, Int ) -> Shape
renderCell gIdx =
    circle lightBlue 50
        |> moveGridIdxToScreen gIdx


moveGridIdxToScreen gIdx =
    uncurry move (gIdxToScreen gIdx)


main =
    game view update init
