module DrawGrid exposing (Config, cells, cellsWithConfig)

import Basics.Extra exposing (uncurry)
import Draw exposing (group, move)
import Grid exposing (GIdx, Grid)
import Svg exposing (Svg)


cells : Float -> (GIdx -> a -> Svg msg) -> Grid a -> Svg msg
cells cw func g =
    let
        ctx =
            toGCtx cw g

        drawCellAt ( gIdx, cell ) =
            [ func gIdx cell ]
                |> group [ moveToGIdx ctx gIdx ]
    in
    Grid.toList g
        |> List.map drawCellAt
        |> group []


type alias Config =
    { move : GIdx -> Draw.Op
    }


cellsWithConfig : Float -> (Config -> GIdx -> a -> Svg msg) -> Grid a -> Svg msg
cellsWithConfig cw func g =
    let
        ctx =
            toGCtx cw g

        config =
            { move = moveToGIdx ctx
            }

        drawCellAt ( gIdx, cell ) =
            func config gIdx cell
    in
    Grid.toList g
        |> List.map drawCellAt
        |> group []



-- GRID CONTEXT


type alias GCtx =
    { cw : Float
    , dx : Float
    , dy : Float
    }


toGCtx : Float -> Grid a -> GCtx
toGCtx gridCellWidth g =
    let
        ( gridWidth, gridHeight ) =
            Grid.wh g |> Tuple.mapBoth (toFloat >> (*) gridCellWidth) (toFloat >> (*) gridCellWidth)

        dx =
            (gridWidth - gridCellWidth) * -0.5

        dy =
            (gridHeight - gridCellWidth) * -0.5
    in
    { cw = gridCellWidth
    , dx = dx
    , dy = dy
    }


moveToGIdx : GCtx -> GIdx -> Draw.Op
moveToGIdx ctx gIdx =
    uncurry move (gIdxToXY ctx gIdx)


gIdxToXY : GCtx -> GIdx -> ( Float, Float )
gIdxToXY { cw, dx, dy } ( xi, yi ) =
    ( toFloat xi * cw + dx, toFloat yi * cw + dy )
