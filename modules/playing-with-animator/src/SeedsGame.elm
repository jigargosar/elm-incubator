module SeedsGame exposing (main)

import Basics.Extra exposing (uncurry)
import Browser exposing (Document)
import Draw exposing (canvas, circle, group, move, rect, square)
import Grid exposing (GI, Grid)
import List.Extra
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
    = Idle (Grid Cell)
    | Connecting (Cons GI) (Grid Cell)


initialGrid : SeedGrid
initialGrid =
    let
        wallIndices =
            [ ( 2, 1 ), ( 5, 1 ), ( 2, 3 ), ( 5, 3 ) ]
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
        |> Idle


type Cell
    = Cell Tile


type Tile
    = Water
    | Wall


type alias Flags =
    { window : Window }


init : Flags -> ( Model, Cmd Msg )
init f =
    ( Model f.window initialGrid
    , Cmd.none
    )


type alias Cons a =
    ( a, List a )



--noinspection ElmUnusedSymbol


makeGICons start fns =
    ( start, List.Extra.scanl (<|) start fns |> List.drop 1 )


moveGI dx dy ( x, y ) =
    ( x + dx, y + dy )



--noinspection ElmUnusedSymbol


giUp =
    moveGI 0 -1



--noinspection ElmUnusedSymbol


giDown =
    moveGI 0 1



--noinspection ElmUnusedSymbol


giLeft =
    moveGI -1 0



--noinspection ElmUnusedSymbol


giRight =
    moveGI 1 0



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )



--noinspection ElmUnusedSymbol


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
        [ canvas
            w
            h
            [ rect "#ffc973" w h []
            , renderGrid model.grid
            ]
        ]


renderGrid : SeedGrid -> Svg msg
renderGrid seedGrid =
    let
        renderCell ctx gi (Cell tile) =
            renderTile ctx.cw [ moveToGI ctx gi ] tile
    in
    case seedGrid of
        Idle grid ->
            gridToListWithCtx renderCell grid
                |> group []

        Connecting _ grid ->
            gridToListWithCtx renderCell grid
                |> group []


renderTile : Float -> List Draw.Op -> Tile -> Svg msg
renderTile cw ops tile =
    case tile of
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


gridToListWithCtx : (GCtx -> GI -> a -> b) -> Grid a -> List b
gridToListWithCtx func grid =
    let
        ctx =
            toGCtx grid
    in
    Grid.toListBy (func ctx) grid


moveToGI : GCtx -> GI -> Draw.Op
moveToGI ctx gi =
    uncurry move (gIdxToXY ctx gi)


gIdxToXY : GCtx -> GI -> ( Float, Float )
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
