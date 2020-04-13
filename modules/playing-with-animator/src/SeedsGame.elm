module SeedsGame exposing (main)

import Basics.Extra exposing (uncurry)
import Browser exposing (Document)
import Draw exposing (canvas, circle, move, rect)
import Grid exposing (GIdx)
import Svg exposing (Svg)



-- Model


type Model
    = M Grid


type alias Grid =
    Grid.Grid Cell


initialGrid : Grid
initialGrid =
    Grid.init 7 5 (\_ -> Water)


type Cell
    = Water


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( M initialGrid
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
view (M g) =
    let
        screenWidth =
            600

        screenHeight =
            800
    in
    Document "SeedsGame"
        [ canvas screenWidth
            screenHeight
            [ rect "#ffc973" screenWidth screenHeight []
            , renderGrid g
            ]
        ]


renderGrid : Grid -> Svg msg
renderGrid g =
    let
        ctx =
            toGCtx g

        renderCellAt ( gIdx, cell ) =
            renderCell ctx gIdx cell
    in
    Grid.toList g
        |> List.map renderCellAt
        |> group


group : List (Svg msg) -> Svg msg
group =
    Svg.g []


renderCell : GCtx -> GIdx -> Cell -> Svg msg
renderCell ({ cw } as ctx) gIdx cell =
    let
        mv =
            moveToGIdx ctx gIdx
    in
    case cell of
        Water ->
            circle "dodgerblue" (cw * 0.25) [ mv ]



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
