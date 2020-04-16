module SeedsGame exposing (main)

import Basics.Extra exposing (uncurry)
import Browser exposing (Document)
import Cons exposing (Cons)
import Draw exposing (canvas, circle, fade, group, move, rect, scale, square)
import Grid exposing (GI, Grid)
import List.Extra
import Process
import Svg exposing (Svg)
import Task



-- SEED GRID


type SeedGrid
    = Idle (Grid Cell)
    | Connecting (Cons GI) (Grid Cell)
    | Collecting TransitionState (Grid Cell)


type alias TransitionState =
    { leaving : Cons GI
    , falling : List ( GI, GI )
    }


type Cell
    = Cell Tile


type Tile
    = Water
    | Wall


initialGrid : SeedGrid
initialGrid =
    let
        wallIndices =
            [ ( 2, 1 ), ( 4, 1 ), ( 2, 3 ), ( 4, 3 ) ]
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



-- GRID INDEX HELPERS


isAdjacentTo ( x1, y1 ) ( x2, y2 ) =
    let
        ( dxa, dya ) =
            ( abs (x1 - x2), abs (y1 - y2) )
    in
    (dxa == 0 && dya == 1) || (dxa == 1 && dya == 0)


moveGI dx dy ( x, y ) =
    ( x + dx, y + dy )



--noinspection ElmUnusedSymbol


makeGICons start fns =
    ( start, List.Extra.scanl (<|) start fns |> List.drop 1 )



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



-- Model


type alias Model =
    { window : Window
    , grid : SeedGrid
    }


type alias Window =
    { width : Float, height : Float }


type alias Flags =
    { window : Window }


init : Flags -> ( Model, Cmd Msg )
init f =
    ( Model f.window initialGrid
    , schedule
        ((connectPath1 |> always connectPath2)
            ++ [ StartCollecting ]
        )
    )


connectPath1 : List Msg
connectPath1 =
    List.Extra.scanl (<|)
        ( 1, 1 )
        [ giDown
        , giLeft
        , identity
        , giRight >> giRight
        , giRight
        , giUp
        , giUp
        , giRight
        , giRight
        , giDown
        , giDown
        , giDown
        , giDown
        , giLeft
        , giLeft
        ]
        |> List.map ToggleConnecting


connectPath2 : List Msg
connectPath2 =
    List.Extra.scanl (<|)
        ( 1, 1 )
        [ giDown
        , giDown
        , giDown
        , giRight
        , giRight
        , giRight
        , giRight
        , giUp
        , giUp
        , giUp
        ]
        |> List.map ToggleConnecting



-- Update


type Msg
    = StartConnecting GI
    | ToggleConnecting GI
    | StartCollecting


areCellAtIndicesConnectionCompatible : GI -> GI -> Grid Cell -> Bool
areCellAtIndicesConnectionCompatible ia ib grid =
    Maybe.map2 (==) (Grid.get ia grid) (Grid.get ib grid)
        |> Maybe.withDefault False


canConnectTo : GI -> Cons GI -> Grid Cell -> Bool
canConnectTo gi connectedIndices grid =
    isAdjacentTo gi (Cons.head connectedIndices)
        && not (Cons.member gi connectedIndices)
        && areCellAtIndicesConnectionCompatible gi (Cons.head connectedIndices) grid


tryStartConnectingAt : GI -> Grid Cell -> Maybe SeedGrid
tryStartConnectingAt =
    let
        initConnecting : GI -> Grid Cell -> SeedGrid
        initConnecting gi grid =
            Connecting (Cons.singleton gi) grid

        canStartConnectionAtGI : GI -> Grid Cell -> Bool
        canStartConnectionAtGI gi grid =
            case Grid.get gi grid of
                Just (Cell tile) ->
                    case tile of
                        Water ->
                            True

                        Wall ->
                            False

                Nothing ->
                    False
    in
    justWhen2 canStartConnectionAtGI initConnecting


justWhen2 pred func v1 v2 =
    if pred v1 v2 then
        Just (func v1 v2)

    else
        Nothing


pushConnectingGI : GI -> Cons GI -> Grid Cell -> SeedGrid
pushConnectingGI gi connectedIndices grid =
    Connecting (Cons.push gi connectedIndices) grid


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( message, model.grid ) of
        ( StartConnecting gi, Idle grid ) ->
            case tryStartConnectingAt gi grid of
                Just ng ->
                    ( setGrid ng model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ( ToggleConnecting gi, Idle grid ) ->
            case tryStartConnectingAt gi grid of
                Just ng ->
                    ( setGrid ng model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ( ToggleConnecting gi, Connecting connectedIndices grid ) ->
            if canConnectTo gi connectedIndices grid then
                ( setGrid (pushConnectingGI gi connectedIndices grid) model
                , Cmd.none
                )

            else if Cons.head connectedIndices == gi then
                case Cons.maybeTail connectedIndices of
                    Just nci ->
                        ( setGrid (Connecting nci grid) model, Cmd.none )

                    Nothing ->
                        ( setGrid (Idle grid) model, Cmd.none )

            else
                ( model, Cmd.none )

        ( StartCollecting, Connecting connectedIndices grid ) ->
            ( setGrid (Collecting (TransitionState connectedIndices []) grid) model, Cmd.none )

        _ ->
            let
                _ =
                    Debug.log "ignoring msg" message
            in
            ( model, Cmd.none )


setGrid grid model =
    { model | grid = grid }



--noinspection ElmUnusedSymbol


defaultDelay =
    100


delayN n msg =
    Process.sleep n |> Task.perform (always msg)


schedule : List b -> Cmd b
schedule =
    List.indexedMap (\i -> delayN (toFloat i * defaultDelay))
        >> Cmd.batch


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
    case seedGrid of
        Idle grid ->
            gridToListWithCtx renderIdleCell grid
                |> group []

        Connecting ciCons grid ->
            gridToListWithCtx (renderConnectingCell ciCons) grid
                |> group []

        Collecting { leaving, falling } grid ->
            let
                renderCell ctx gi =
                    if Cons.member gi leaving then
                        renderLeavingCell ctx gi

                    else
                        renderIdleCell ctx gi
            in
            gridToListWithCtx renderCell grid
                |> group []


gridToListWithCtx : (GCtx -> GI -> a -> b) -> Grid a -> List b
gridToListWithCtx func grid =
    Grid.toListBy (func (toGCtx grid)) grid


renderConnectingCell : Cons GI -> GCtx -> GI -> Cell -> Svg msg
renderConnectingCell ciCons ctx gi =
    if Cons.member gi ciCons then
        renderConnectedCell ctx gi

    else
        renderIdleCell ctx gi


renderIdleCell : GCtx -> GI -> Cell -> Svg msg
renderIdleCell ctx gi (Cell tile) =
    group [ moveToGI ctx gi ] [ renderTile ctx tile ]


renderConnectedCell : GCtx -> GI -> Cell -> Svg msg
renderConnectedCell ctx gi (Cell tile) =
    group [ moveToGI ctx gi, scale 0.6 ] [ renderTile ctx tile ]


renderLeavingCell : GCtx -> GI -> Cell -> Svg msg
renderLeavingCell ctx _ (Cell tile) =
    group [ move 0 -300, scale 0.1, fade 0.1 ] [ renderTile ctx tile ]


renderTile : GCtx -> Tile -> Svg msg
renderTile { cw } tile =
    case tile of
        Water ->
            circle "dodgerblue" (cw * 0.25) []

        Wall ->
            square "brown" (cw * 0.7) []



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
