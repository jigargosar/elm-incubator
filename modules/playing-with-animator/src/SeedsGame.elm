module SeedsGame exposing (main)

import Basics.Extra exposing (uncurry)
import Browser exposing (Document)
import Draw exposing (canvas, circle, fade, group, move, rect, scale)
import DrawGrid
import Grid exposing (GIdx)
import Svg exposing (Svg)



-- Model


type alias Model =
    { window : Window
    , grid : Grid
    }


type alias Window =
    { width : Float, height : Float }


type alias Grid =
    Grid.Grid Cell


initialGrid : Grid
initialGrid =
    Grid.init 7 5 (\_ -> Water)


type Cell
    = Water


type alias Flags =
    { window : Window }


init : Flags -> ( Model, Cmd Msg )
init f =
    ( Model f.window initialGrid
    , Cmd.none
    )



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


type alias DM =
    Document Msg


view : Model -> DM
view model =
    let
        w =
            model.window.width

        h =
            model.window.height
    in
    Document "SeedsGame"
        [ canvas w
            h
            [ rect "#ffc973" w h []
            , renderGrid model.grid
            ]
        ]


renderGrid : Grid -> Svg msg
renderGrid g =
    let
        ctx =
            toGCtx g
    in
    [ DrawGrid.cells ctx.cw (renderCell ctx) g ]
        |> group [ fade 0.2, scale 0.5 ]


renderCell : GCtx -> GIdx -> Cell -> Svg msg
renderCell { cw } _ cell =
    case cell of
        Water ->
            circle "dodgerblue" (cw * 0.25) [ fade 1 ]



-- GRID CONTEXT


type alias GCtx =
    { cw : Float
    , dx : Float
    , dy : Float
    }


toGCtx : Grid -> GCtx
toGCtx g =
    let
        gridCellWidth =
            50

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



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
