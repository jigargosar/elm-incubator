module SeedsGame exposing (main)

import Basics.Extra exposing (uncurry)
import Browser exposing (Document)
import Draw exposing (canvas, circle, group, move, rect, square)
import Grid exposing (GIdx, Grid)
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
    , grid : SeedGrid
    }


type alias Window =
    { width : Float, height : Float }


type SeedGrid
    = SG (Grid.Grid Cell)


initialGrid : SeedGrid
initialGrid =
    let
        wallIndices =
            [ ( 1, 1 ), ( 2, 1 ) ]
                |> List.map (moveGIdxInDir Down >> moveGIdxInDir Right)
    in
    Grid.init
        7
        5
        (\i ->
            if List.member i wallIndices then
                Cell Wall

            else
                Cell Water
        )
        |> SG


type Cell
    = Cell Sprite


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
            (lcrFromNEList
                (makeGIdxCons ( 1, 1 )
                    [ Right
                    , Right
                    , Right
                    , Down
                    , Down
                    , Left
                    , Left
                    ]
                )
            )
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



--lcrOppDir dir =
--    case dir of
--        Back ->
--            Forth
--
--        Forth ->
--            Back
--


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


gridToggleConnected : GIdx -> SeedGrid -> Maybe SeedGrid
gridToggleConnected _ _ =
    Nothing


gridCollectConnected : SeedGrid -> SeedGrid
gridCollectConnected =
    identity


gridFallIdle : SeedGrid -> SeedGrid
gridFallIdle =
    identity


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


renderGrid : SeedGrid -> Svg msg
renderGrid (SG g) =
    let
        ctx =
            toGCtx g

        drawCellAt ( gIdx, cell ) =
            renderCell ctx gIdx cell
    in
    Grid.toList g
        |> List.map drawCellAt
        |> group []


renderCell : GCtx -> GIdx -> Cell -> Svg msg
renderCell ctx gIdx cell =
    case cell of
        Cell sprite ->
            group []
                [ renderSprite ctx.cw [ moveToGIdx ctx gIdx ] sprite
                ]


moveToGIdx : GCtx -> GIdx -> Draw.Op
moveToGIdx ctx gIdx =
    uncurry move (gIdxToXY ctx gIdx)


gIdxToXY : GCtx -> GIdx -> ( Float, Float )
gIdxToXY { cw, dx, dy } ( xi, yi ) =
    ( toFloat xi * cw + dx, toFloat yi * cw + dy )


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


toGCtx : Grid a -> GCtx
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
