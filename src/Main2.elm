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


gridXYLeftTop : ( Float, Float )
gridXYLeftTop =
    ( (toFloat gridXLength * gridCellWidth - gridCellWidth) * -0.5
    , (toFloat gridYLength * gridCellWidth - gridCellWidth) * -0.5
    )


gridPositions : List ( Int, Int )
gridPositions =
    List.range 0 (gridXLength - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (gridYLength - 1) |> List.map (Tuple.pair x)
            )


gIdxToScreen : ( Int, Int ) -> ( Float, Float )
gIdxToScreen ( x_, y_ ) =
    let
        x =
            toFloat x_

        y =
            toFloat y_

        ( dx, dy ) =
            gridXYLeftTop
    in
    ( x * gridCellWidth + dx, y * gridCellWidth + dy )


update : Computer -> Mem -> Mem
update computer mem =
    mem


view : Computer -> Mem -> List Shape
view computer mem =
    [ group (List.map renderWaterCell gridPositions)
    ]


waterRadius =
    gridCellWidth * 0.4


renderWaterCell : ( Int, Int ) -> Shape
renderWaterCell gIdx =
    circle lightBlue waterRadius
        |> moveGridIdxToScreen gIdx


moveGridIdxToScreen gIdx =
    uncurry move (gIdxToScreen gIdx)


main =
    game view update init
