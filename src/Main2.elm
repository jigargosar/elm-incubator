module Main2 exposing (main)

import Basics.Extra exposing (flip, uncurry)
import Dict exposing (Dict)
import Playground exposing (..)



-- Game Scaffold


type Mem
    = Mem GridCells


type alias GridCells =
    Dict ( Int, Int ) Cell


initialGridCells : GridCells
initialGridCells =
    gridPositions
        |> List.map (flip Tuple.pair Static)
        |> Dict.fromList


type Cell
    = Static
    | Growing
    | Shrinking


init : Mem
init =
    Mem initialGridCells


gridXLength =
    7


gridYLength =
    7


gridCellWidth =
    70


gridXYLeftTop : ( Float, Float )
gridXYLeftTop =
    ( (toFloat gridXLength * gridCellWidth - gridCellWidth) * -0.5
    , (toFloat gridYLength * gridCellWidth - gridCellWidth) * -0.5
    )


gIdxToScreen : ( Int, Int ) -> ( Float, Float )
gIdxToScreen ( x, y ) =
    let
        ( dx, dy ) =
            gridXYLeftTop
    in
    ( toFloat x * gridCellWidth + dx, toFloat y * gridCellWidth + dy )


isGIdxValid : ( Int, Int ) -> Bool
isGIdxValid ( x, y ) =
    x >= 0 && x <= gridXLength && y >= 0 && y < gridYLength


validGIdx : ( Int, Int ) -> Maybe ( Int, Int )
validGIdx gIdx =
    if isGIdxValid gIdx then
        Just gIdx

    else
        Nothing


screenToGIdx : Float -> Float -> Maybe ( Int, Int )
screenToGIdx x y =
    let
        ( dx, dy ) =
            gridXYLeftTop
    in
    ( round ((x - dx) / gridCellWidth), round ((y - dy) / gridCellWidth) )
        |> validGIdx


computerToGIdx : Computer -> Maybe ( Int, Int )
computerToGIdx { mouse } =
    screenToGIdx mouse.x mouse.y


gridPositions : List ( Int, Int )
gridPositions =
    List.range 0 (gridXLength - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (gridYLength - 1) |> List.map (Tuple.pair x)
            )


update : Computer -> Mem -> Mem
update computer (Mem gridCells) =
    let
        maybeMouseGIdx =
            computerToGIdx computer
    in
    Dict.map
        (\gIdx cellState ->
            if Just gIdx == maybeMouseGIdx then
                Shrinking

            else if cellState == Shrinking then
                Static

            else
                cellState
        )
        gridCells
        |> Mem


view : Computer -> Mem -> List Shape
view computer (Mem gridCells) =
    [ group (List.map renderWaterCell (Dict.toList gridCells))
    ]


renderWaterCell : ( ( Int, Int ), Cell ) -> Shape
renderWaterCell ( gIdx, cell ) =
    circle lightBlue waterRadius
        |> moveGridIdxToScreen gIdx
        |> scale
            (case cell of
                Static ->
                    1

                Growing ->
                    1

                Shrinking ->
                    0.5
            )


waterRadius =
    gridCellWidth * 0.4


moveGridIdxToScreen gIdx =
    uncurry move (gIdxToScreen gIdx)


main =
    game view update init
