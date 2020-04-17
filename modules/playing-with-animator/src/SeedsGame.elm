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


type GridState
    = GridState (Grid Cell) GridSubState


type GridSubState
    = GridIdle
    | GridConnecting GI GridConnectingSubState
    | GridCollecting TransitionState


type GridConnectingSubState
    = GridConnectingSingle
    | GridConnectingAtLeastTwo GI (List GI)


type alias TransitionState =
    { leaving : Cons GI
    , falling : List ( GI, GI )
    }


type Cell
    = Cell Tile


type Tile
    = Water
    | Wall


initialGrid : GridState
initialGrid =
    let
        wallIndices =
            [ ( 2, 1 ), ( 4, 1 ), ( 2, 3 ), ( 4, 3 ) ]

        grid =
            Grid.init
                7
                5
                (\i ->
                    if List.member i wallIndices then
                        Cell Wall

                    else
                        Cell Water
                )
    in
    GridState grid GridIdle


areCellAtIndicesConnectionCompatible : GI -> GI -> Grid Cell -> Bool
areCellAtIndicesConnectionCompatible ia ib grid =
    Maybe.map2 (==) (Grid.get ia grid) (Grid.get ib grid)
        |> Maybe.withDefault False


startConnecting : GI -> Grid Cell -> Maybe GridState
startConnecting =
    let
        initConnecting : GI -> Grid Cell -> GridState
        initConnecting gi grid =
            GridConnecting gi GridConnectingSingle
                |> GridState grid

        canStartConnectionAt : GI -> Grid Cell -> Bool
        canStartConnectionAt gi grid =
            case Grid.get gi grid of
                Just (Cell tile) ->
                    case tile of
                        Water ->
                            True

                        Wall ->
                            False

                Nothing ->
                    False

        do gi grid =
            if canStartConnectionAt gi grid then
                Just (initConnecting gi grid)

            else
                Nothing
    in
    do


isAdjacentTo ( x1, y1 ) ( x2, y2 ) =
    let
        ( dxa, dya ) =
            ( abs (x1 - x2), abs (y1 - y2) )
    in
    (dxa == 0 && dya == 1) || (dxa == 1 && dya == 0)


giMove dx dy ( x, y ) =
    ( x + dx, y + dy )


giUp =
    giMove 0 -1


giDown =
    giMove 0 1


giLeft =
    giMove -1 0


giRight =
    giMove 1 0



-- Model


type State
    = State Window GridState


type alias Window =
    { width : Float, height : Float }


type alias Flags =
    { window : Window }


init : Flags -> ( State, Cmd Msg )
init f =
    ( State f.window initialGrid
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


update : Msg -> State -> ( State, Cmd Msg )
update message ((State _ (GridState grid gs)) as model) =
    case ( message, gs ) of
        ( StartConnecting gi, GridIdle ) ->
            case startConnecting gi grid of
                Just ng ->
                    ( setGridState ng model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ( ToggleConnecting gi, GridIdle ) ->
            case startConnecting gi grid of
                Just ng ->
                    ( setGridState ng model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ( ToggleConnecting gi, GridConnecting lastGI GridConnectingSingle ) ->
            if
                isAdjacentTo gi lastGI
                    && areCellAtIndicesConnectionCompatible gi lastGI grid
            then
                ( setGridState
                    (GridConnecting gi (GridConnectingAtLeastTwo lastGI [])
                        |> GridState grid
                    )
                    model
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ( ToggleConnecting gi, GridConnecting lastGI (GridConnectingAtLeastTwo secondLast remaining) ) ->
            if gi == secondLast then
                case remaining of
                    [] ->
                        ( setGridState
                            (GridConnecting secondLast GridConnectingSingle
                                |> GridState grid
                            )
                            model
                        , Cmd.none
                        )

                    thirdLast :: newRemaining ->
                        ( setGridState
                            (GridConnecting secondLast (GridConnectingAtLeastTwo thirdLast newRemaining)
                                |> GridState grid
                            )
                            model
                        , Cmd.none
                        )

            else if
                isAdjacentTo gi lastGI
                    && areCellAtIndicesConnectionCompatible gi lastGI grid
            then
                ( setGridState
                    (GridConnecting gi (GridConnectingAtLeastTwo lastGI (secondLast :: remaining))
                        |> GridState grid
                    )
                    model
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ( StartCollecting, GridConnecting lastGI (GridConnectingAtLeastTwo secondLastGI remainingGI) ) ->
            ( setGridState
                (GridCollecting (TransitionState (Cons.cons lastGI (secondLastGI :: remainingGI)) [])
                    |> GridState grid
                )
                model
            , Cmd.none
            )

        _ ->
            let
                _ =
                    Debug.log "ignoring msg" message
            in
            ( model, Cmd.none )


setGridState : GridState -> State -> State
setGridState gs (State win _) =
    State win gs



--noinspection ElmUnusedSymbol


defaultDelay =
    100


delayN n msg =
    Process.sleep n |> Task.perform (always msg)


schedule : List b -> Cmd b
schedule =
    List.indexedMap (\i -> delayN (toFloat i * defaultDelay))
        >> Cmd.batch


subscriptions : State -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


type alias DM =
    Document Msg


view : State -> DM
view (State window gs) =
    let
        w =
            window.width

        h =
            window.height
    in
    Document "SeedsGame"
        [ canvas
            w
            h
            [ rect "#ffc973" w h []
            , renderGrid gs
            ]
        ]


renderGrid : GridState -> Svg msg
renderGrid (GridState grid gs) =
    case gs of
        GridIdle ->
            gridToListWithCtx renderIdleCell grid
                |> group []

        GridConnecting gi gridConnectingSS ->
            let
                ciCons =
                    case gridConnectingSS of
                        GridConnectingSingle ->
                            Cons.singleton gi

                        GridConnectingAtLeastTwo gi2 list ->
                            Cons.cons gi (gi2 :: list)
            in
            gridToListWithCtx (renderConnectingCell ciCons) grid
                |> group []

        GridCollecting { leaving, falling } ->
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


main : Program Flags State Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
