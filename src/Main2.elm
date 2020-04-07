module Main2 exposing (main)

import Basics.Extra exposing (uncurry)
import Playground exposing (..)



-- Game Scaffold


type Mem
    = Mem


init : Mem
init =
    Mem


gridWidth =
    10


gridHeight =
    10


gridPositions : List ( Int, Int )
gridPositions =
    List.range 0 (gridWidth - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (gridHeight - 1) |> List.map (Tuple.pair x)
            )


update : Computer -> Mem -> Mem
update computer mem =
    mem


view : Computer -> Mem -> List Shape
view computer mem =
    [ group (List.map renderCell gridPositions)
    ]


gIdxToScreen : ( Int, Int ) -> ( Float, Float )
gIdxToScreen ( x, y ) =
    ( toFloat x * 50, toFloat y * 50 )


renderCell : ( Int, Int ) -> Shape
renderCell gIdx =
    circle lightBlue 50
        |> uncurry move (gIdxToScreen gIdx)


main =
    game view update init
