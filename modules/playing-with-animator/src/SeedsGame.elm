module SeedsGame exposing (main)

import Browser exposing (Document)
import Draw exposing (canvas, circle, fade, group, rect, rotate, scale)
import DrawGrid
import Grid exposing (GIdx)
import Process
import Svg exposing (Svg)
import Task



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
    Grid.init 7 5 (\_ -> Cell CS_Idle Water)


type Cell
    = Cell CS Sprite


type CS
    = CS_Connected
    | CS_Idle


type Sprite
    = Water


type alias Flags =
    { window : Window }


init : Flags -> ( Model, Cmd Msg )
init f =
    ( Model f.window initialGrid
    , Process.sleep 1000 |> Task.perform (always Foo)
    )



-- Update


type Msg
    = NoOp
    | Foo


cellToggleConnected : Cell -> Cell
cellToggleConnected (Cell cs s) =
    Cell
        (case cs of
            CS_Connected ->
                CS_Idle

            CS_Idle ->
                CS_Connected
        )
        s


gridToggleConnected : GIdx -> Grid -> Maybe Grid
gridToggleConnected idx =
    Grid.mapIdx idx cellToggleConnected


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Foo ->
            ( { model
                | grid =
                    gridToggleConnected ( 0, 0 ) model.grid
                        |> Maybe.withDefault model.grid
              }
            , Process.sleep 1000 |> Task.perform (always Foo)
            )


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
        |> group [ fade 1, scale 1, rotate 0 ]


renderCell : GCtx -> GIdx -> Cell -> Svg msg
renderCell { cw } _ cell =
    case cell of
        Cell cs sprite ->
            group []
                [ group (renderCS cs) [ renderSprite cw sprite ]
                , group (renderCS2 cs) [ renderSprite cw sprite ]
                ]


renderCS : CS -> List Draw.Op
renderCS cs =
    case cs of
        CS_Connected ->
            [ scale 0.5 ]

        CS_Idle ->
            []


renderCS2 : CS -> List Draw.Op
renderCS2 cs =
    case cs of
        CS_Connected ->
            [ fade 0, scale 2 ]

        CS_Idle ->
            []


renderSprite : Float -> Sprite -> Svg msg
renderSprite cw sprite =
    case sprite of
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



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
