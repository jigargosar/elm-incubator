module SeedsGame exposing (main)

import Basics.Extra exposing (uncurry)
import Browser exposing (Document)
import Draw exposing (canvas, circle, fade, group, move, rect, rotate, scale, square)
import DrawGrid
import Grid exposing (GIdx)
import Html exposing (div, text)
import Html.Attributes as A
import List.Extra
import Pivot exposing (Pivot)
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
    let
        wallIndices =
            [ ( 1, 1 ), ( 2, 1 ) ]
    in
    Grid.init
        7
        5
        (\i ->
            if List.member i wallIndices then
                Cell CS_Static Wall

            else
                Cell CS_Idle Water
        )


type Cell
    = Cell CS Sprite


type CS
    = CS_Connected
    | CS_Idle
    | CS_Static
    | CS_MovingToWaterCollector
    | CS_IdleFallingTo GIdx


type Sprite
    = Water
    | Wall


type alias Flags =
    { window : Window }


init : Flags -> ( Model, Cmd Msg )
init f =
    ( Model f.window initialGrid
    , delay
        (Foo Forth
            (lcrFromNEList (makeGIdxCons ( 0, 0 ) [ Right, Right, Right, Down, Down, Left, Left ]))
        )
    )


type alias Cons a =
    ( a, List a )


makeGIdxCons : GIdx -> List FourD -> Cons GIdx
makeGIdxCons start fourDS =
    ( start, List.Extra.scanl moveGIdxInDir start fourDS |> List.drop 1 )


moveGIdx dx dy ( x, y ) =
    ( x + dx, y + dy )


moveGIdxInDir : FourD -> GIdx -> GIdx
moveGIdxInDir fourD =
    case fourD of
        Up ->
            moveGIdx 0 -1

        Down ->
            moveGIdx 0 1

        Left ->
            moveGIdx -1 0

        Right ->
            moveGIdx 1 0


type FourD
    = Up
    | Down
    | Left
    | Right



-- LCR


lcrFromNEList =
    uncurry Pivot.fromCons


lcrC =
    Pivot.getC


type LCRDir
    = Back
    | Forth


lcrOppDir dir =
    case dir of
        Back ->
            Forth

        Forth ->
            Back


lcrGo dir =
    case dir of
        Back ->
            Pivot.goL

        Forth ->
            Pivot.goR



-- Update


type Msg
    = NoOp
    | Foo LCRDir (Pivot GIdx)
    | Collect
    | FallIdle


cellToggleConnected : Cell -> Cell
cellToggleConnected (Cell cs s) =
    Cell
        (case cs of
            CS_Connected ->
                CS_Idle

            CS_Idle ->
                CS_Connected

            CS_MovingToWaterCollector ->
                CS_MovingToWaterCollector

            CS_IdleFallingTo gIdx ->
                CS_IdleFallingTo gIdx

            CS_Static ->
                CS_Static
        )
        s


cellCollectConnect : Cell -> Cell
cellCollectConnect (Cell cs s) =
    Cell
        (case cs of
            CS_Connected ->
                CS_MovingToWaterCollector

            CS_Idle ->
                CS_Idle

            CS_MovingToWaterCollector ->
                CS_MovingToWaterCollector

            CS_IdleFallingTo gIdx ->
                CS_IdleFallingTo gIdx

            CS_Static ->
                CS_Static
        )
        s


gridToggleConnected : GIdx -> Grid -> Maybe Grid
gridToggleConnected idx =
    Grid.mapIdx idx cellToggleConnected


gridCollectConnected : Grid -> Grid
gridCollectConnected =
    Grid.map (always cellCollectConnect)


gridFallIdle : Grid -> Grid
gridFallIdle g =
    let
        getHolesBelow i holesCt =
            let
                newI =
                    moveGIdxInDir Down i
            in
            case Grid.get newI g of
                Just (Cell CS_MovingToWaterCollector _) ->
                    getHolesBelow newI (holesCt + 1)

                _ ->
                    holesCt

        func i ((Cell cs s) as cell) =
            if cs == CS_Idle then
                let
                    holes =
                        getHolesBelow i 0
                in
                if holes > 0 then
                    Cell (CS_IdleFallingTo (moveGIdx 0 holes i)) s

                else
                    cell

            else
                cell
    in
    Grid.map func g


modelToggleConnected idx model =
    { model
        | grid =
            gridToggleConnected idx model.grid
                |> Maybe.withDefault model.grid
    }


modelCollectConnected model =
    { model
        | grid =
            gridCollectConnected model.grid
    }


modelFallIdle model =
    { model
        | grid =
            gridFallIdle model.grid
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Collect ->
            ( modelCollectConnected model, delay FallIdle )

        Foo dir lcr ->
            ( modelToggleConnected (lcrC lcr) model
            , delay <|
                case lcrGo dir lcr of
                    Just nLCR ->
                        Foo dir nLCR

                    Nothing ->
                        Collect
            )

        FallIdle ->
            ( modelFallIdle model, Cmd.none )


delay msg =
    Process.sleep 100 |> Task.perform (always msg)


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
        [ div [ A.id "un-caught-error-container", A.class "fixed absolute--fill z-max" ]
            [ text "Uncaught error" ]
            |> always (text "")
        , canvas w
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
    [ DrawGrid.cellsWithConfig ctx.cw (renderCell ctx) g ]
        |> group [ fade 1, scale 1, rotate 0 ]


renderCell : GCtx -> DrawGrid.Config -> GIdx -> Cell -> Svg msg
renderCell { cw } conf gIdx cell =
    case cell of
        Cell cs sprite ->
            group []
                [ renderSprite cw (renderCS conf gIdx cs) sprite
                , renderSprite cw (renderCS2 conf gIdx cs) sprite
                ]


renderCS : DrawGrid.Config -> GIdx -> CS -> List Draw.Op
renderCS conf gIdx cs =
    case cs of
        CS_Connected ->
            [ conf.move gIdx, scale 0.5 ]

        CS_Idle ->
            [ conf.move gIdx ]

        CS_MovingToWaterCollector ->
            [ moveToWaterCollector, fade 0.1, scale 0.1 ]

        CS_IdleFallingTo toGIdx ->
            [ conf.move toGIdx ]

        CS_Static ->
            [ conf.move gIdx ]


moveToWaterCollector =
    move 0 -300


renderCS2 : DrawGrid.Config -> GIdx -> CS -> List Draw.Op
renderCS2 conf gIdx cs =
    case cs of
        CS_Connected ->
            [ conf.move gIdx, fade 0, scale 2 ]

        CS_Idle ->
            [ conf.move gIdx ]

        CS_MovingToWaterCollector ->
            [ conf.move gIdx, fade 0.1 ]

        CS_IdleFallingTo toGIdx ->
            [ conf.move toGIdx ]

        CS_Static ->
            [ conf.move gIdx ]


renderSprite : Float -> List Draw.Op -> Sprite -> Svg msg
renderSprite cw ops sprite =
    case sprite of
        Water ->
            circle "dodgerblue" (cw * 0.25) ops

        Wall ->
            square "brown" (cw * 0.8) ops



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
