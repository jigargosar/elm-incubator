module SeedsGameV1 exposing (main)

import Basics.Extra exposing (uncurry)
import Browser exposing (Document)
import Dict exposing (Dict)
import Draw exposing (canvas, circle, fade, group, move, noTransition, rect, scale, square, transition)
import Grid exposing (GI, Grid)
import List.Extra
import Maybe.Extra
import Process
import Set exposing (Set)
import Svg exposing (Svg)
import Task



-- SEED GRID


type SeedsGrid
    = SeedsGrid (Grid Cell) GridState


type Cell
    = Cell Tile


type Tile
    = Water
    | Wall


type GridState
    = GridIdle
    | GridConnecting ConnectingState
    | GridLeavingFalling TransitionState
    | GridGeneratingStart TransitionState
    | GridGenerating TransitionState



-- CONNECTING STATE HELPERS


type ConnectingState
    = ConnectingState GI (List GI)


startConnection : GI -> Grid Cell -> Maybe ConnectingState
startConnection gi grid =
    case canStartConnectionAt gi grid of
        True ->
            Just (ConnectingState gi [])

        False ->
            Nothing


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


extendConnection : GI -> Grid Cell -> ConnectingState -> Maybe ConnectingState
extendConnection gi grid (ConnectingState lastGI remaining) =
    if areConnectable gi lastGI grid && not (List.member gi remaining) then
        Just (ConnectingState gi (lastGI :: remaining))

    else
        Nothing


areConnectable : GI -> GI -> Grid Cell -> Bool
areConnectable aa bb grid =
    areCellsAtIndicesConnectable aa bb grid && isAdjacentTo aa bb


areCellsAtIndicesConnectable : GI -> GI -> Grid Cell -> Bool
areCellsAtIndicesConnectable ia ib grid =
    Maybe.map2 (==) (Grid.get ia grid) (Grid.get ib grid)
        |> Maybe.withDefault False


shrinkConnection : GI -> ConnectingState -> Maybe ConnectingState
shrinkConnection gi (ConnectingState _ oldRemaining) =
    case oldRemaining of
        [] ->
            Nothing

        last :: newRemaining ->
            if last == gi then
                Just (ConnectingState last newRemaining)

            else
                Nothing



-- COMPUTE FALLING INDICES : List (from,to)


type alias TransitionState =
    { leaving : Set GI
    , falling : Dict GI GI
    , generated : Set GI
    }


computeFalling : Grid Cell -> List GI -> List ( GI, GI )
computeFalling grid =
    let
        firstMovableIdxAbove : GI -> List GI -> Maybe GI
        firstMovableIdxAbove startIdx emptyIndices =
            entriesAbove startIdx grid
                |> List.Extra.find
                    (\( idx, cell ) ->
                        not (List.member idx emptyIndices) && isCellMovable cell
                    )
                |> Maybe.map Tuple.first

        nextFalling emptyIndices =
            case unconsMax emptyIndices of
                Nothing ->
                    Nothing

                Just ( destIdx, pendingEmptyIndices ) ->
                    case firstMovableIdxAbove destIdx pendingEmptyIndices of
                        Nothing ->
                            nextFalling pendingEmptyIndices

                        Just srcIdx ->
                            Just
                                ( ( srcIdx, destIdx )
                                , srcIdx :: pendingEmptyIndices
                                )
    in
    List.Extra.unfoldr nextFalling


isCellMovable : Cell -> Bool
isCellMovable (Cell s) =
    case s of
        Water ->
            True

        Wall ->
            False


unconsMax : List comparable -> Maybe ( comparable, List comparable )
unconsMax l =
    List.maximum l
        |> Maybe.map (\m -> ( m, List.Extra.remove m l ))


entriesAbove : GI -> Grid a -> List ( GI, a )
entriesAbove si g =
    List.Extra.unfoldr
        (upOf
            >> (\i ->
                    Grid.get i g
                        |> Maybe.map (\c -> ( ( i, c ), i ))
               )
        )
        si



-- GRID INDEX HELPERS


isAdjacentTo ( x1, y1 ) ( x2, y2 ) =
    let
        ( dxa, dya ) =
            ( abs (x1 - x2), abs (y1 - y2) )
    in
    (dxa == 0 && dya == 1) || (dxa == 1 && dya == 0)


moveGIBy dx dy ( x, y ) =
    ( x + dx, y + dy )


upOf =
    moveGIBy 0 -1


downOf =
    moveGIBy 0 1


leftOf =
    moveGIBy -1 0


rightOf =
    moveGIBy 1 0



-- INITIAL GRID


initialGrid : SeedsGrid
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
    SeedsGrid grid GridIdle



-- Model


type Model
    = Model Window SeedsGrid


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


schedule : List b -> Cmd b
schedule =
    List.indexedMap (\i -> delayN (toFloat i * defaultDelay))
        >> Cmd.batch


defaultDelay =
    100


delayN n msg =
    Process.sleep n |> Task.perform (always msg)


connectPath1 : List Msg
connectPath1 =
    List.Extra.scanl (<|)
        ( 1, 1 )
        [ downOf
        , leftOf
        , identity
        , rightOf >> rightOf
        , rightOf
        , upOf
        , upOf
        , rightOf
        , rightOf
        , downOf
        , downOf
        , downOf
        , downOf
        , leftOf
        , leftOf
        ]
        |> List.map ToggleConnecting


connectPath2 : List Msg
connectPath2 =
    List.Extra.scanl (<|)
        ( 1, 1 )
        [ downOf
        , downOf
        , downOf
        , rightOf
        , rightOf
        , rightOf
        , rightOf
        , upOf
        , upOf
        , upOf
        ]
        |> List.map ToggleConnecting



-- Update


type Msg
    = ToggleConnecting GI
    | StartCollecting
    | StartGenerating
    | ContinueGenerating
    | StopGenerating


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        return =
            customUpdate message model
    in
    case return of
        SetGridState gridState cmd ->
            ( setGridState gridState model, cmd )

        Stay ->
            ( model, Cmd.none )


setSeedsGrid : SeedsGrid -> Model -> Model
setSeedsGrid gs (Model win _) =
    Model win gs


setGridState : GridState -> Model -> Model
setGridState gridState ((Model _ (SeedsGrid grid _)) as model) =
    setSeedsGrid (SeedsGrid grid gridState) model


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- CUSTOM UPDATE, i.e. using Return type


type Return
    = SetGridState GridState (Cmd Msg)
    | Stay


customUpdate : Msg -> Model -> Return
customUpdate message (Model _ (SeedsGrid grid gs)) =
    case message of
        ToggleConnecting gi ->
            case gs of
                GridIdle ->
                    maybeGotoConnecting (startConnection gi grid)

                GridConnecting connectingState ->
                    maybeGotoConnecting
                        (Maybe.Extra.oneOf
                            [ extendConnection gi grid
                            , shrinkConnection gi
                            ]
                            connectingState
                        )

                _ ->
                    Stay

        StartCollecting ->
            case gs of
                GridConnecting (ConnectingState lastGI ((_ :: _) as nonEmptyList)) ->
                    let
                        leaving =
                            lastGI
                                :: nonEmptyList
                                |> Set.fromList

                        falling : Dict GI GI
                        falling =
                            computeFalling grid (Set.toList leaving)
                                |> Dict.fromList

                        newEmpty =
                            Dict.keys falling |> Set.fromList

                        newFilled =
                            Dict.values falling |> Set.fromList

                        generated =
                            Set.diff (Set.union newEmpty leaving) newFilled
                    in
                    SetGridState
                        (GridLeavingFalling
                            { leaving = leaving
                            , falling = falling
                            , generated = generated
                            }
                        )
                        (delayN (defaultDelay * 15) StartGenerating)

                _ ->
                    Stay

        StartGenerating ->
            case gs of
                GridLeavingFalling transition ->
                    SetGridState (GridGeneratingStart transition) (delayN 1 ContinueGenerating)

                _ ->
                    Stay

        ContinueGenerating ->
            case gs of
                GridGeneratingStart transition ->
                    SetGridState
                        (GridGenerating transition)
                        (delayN (defaultDelay * 10) StopGenerating |> always Cmd.none)

                _ ->
                    Stay

        StopGenerating ->
            case gs of
                GridGenerating _ ->
                    SetGridState GridIdle Cmd.none

                _ ->
                    Stay


maybeGoto : (a -> Return) -> Maybe a -> Return
maybeGoto func maybeVal =
    case maybeVal of
        Nothing ->
            Stay

        Just val ->
            func val


gotoGridState : GridState -> Return
gotoGridState state =
    SetGridState state Cmd.none


gotoConnecting : ConnectingState -> Return
gotoConnecting cs =
    gotoGridState (GridConnecting cs)


maybeGotoConnecting : Maybe ConnectingState -> Return
maybeGotoConnecting =
    maybeGoto gotoConnecting



-- View


type alias DM =
    Document Msg


view : Model -> DM
view (Model window gs) =
    let
        w =
            window.width

        h =
            window.height
    in
    Document "SeedsGameV1"
        [ canvas
            w
            h
            [ rect "#ffc973" w h []
            , renderGrid gs
            ]
        ]


renderGrid : SeedsGrid -> Svg msg
renderGrid (SeedsGrid grid gs) =
    let
        ctx =
            toGCtx grid

        renderGridCellsWith renderCell =
            group [] (Grid.toListBy renderCell grid)
    in
    case gs of
        GridIdle ->
            renderGridCellsWith (renderIdleCell ctx)

        GridConnecting cs ->
            let
                connectedIndices =
                    case cs of
                        ConnectingState lastIdx remainingIdx ->
                            lastIdx :: remainingIdx

                isConnected i =
                    List.member i connectedIndices

                renderCell : GI -> Cell -> Svg msg
                renderCell idx =
                    if isConnected idx then
                        renderConnectedCell ctx idx

                    else
                        renderIdleCell ctx idx
            in
            renderGridCellsWith renderCell

        GridLeavingFalling { leaving, falling } ->
            let
                isLeaving idx =
                    Set.member idx leaving

                renderCell idx =
                    case Dict.get idx falling of
                        Just to ->
                            renderIdleCell ctx to

                        Nothing ->
                            if isLeaving idx then
                                renderLeavingCell ctx idx

                            else
                                renderIdleCell ctx idx
            in
            renderGridCellsWith renderCell

        GridGeneratingStart { generated } ->
            let
                renderCell idx =
                    if Set.member idx generated then
                        renderGeneratedStart ctx idx

                    else
                        renderIdleCellInstant ctx idx
            in
            renderGridCellsWith renderCell

        GridGenerating _ ->
            renderGridCellsWith (renderGeneratedEnd ctx)


renderIdleCell : GCtx -> GI -> Cell -> Svg msg
renderIdleCell ctx gi (Cell tile) =
    group [ moveToGI ctx gi ] [ renderTile ctx tile ]


renderIdleCellInstant : GCtx -> GI -> Cell -> Svg msg
renderIdleCellInstant ctx gi (Cell tile) =
    group [ moveToGI ctx gi, noTransition ] [ renderTile ctx tile ]


renderGeneratedEnd : GCtx -> GI -> Cell -> Svg msg
renderGeneratedEnd ctx gi (Cell tile) =
    group
        [ moveToGI ctx gi
        , transition "all 500ms cubic-bezier(0.59, 0.87, 0.5, 1.31)"
        , transition "all 500ms "
        , [ "transform 500ms cubic-bezier(0.5, 1.5, 0.5, 1.5)"
          , "opacity 500ms cubic-bezier(0.55, 0.06, 0.68, 0.19)"
          , "opacity 500ms ease-in"
          ]
            |> String.join ","
            |> transition
        ]
        [ renderTile ctx tile ]


renderGeneratedStart : GCtx -> GI -> Cell -> Svg msg
renderGeneratedStart ctx gi (Cell tile) =
    group [ moveToGI ctx gi, move 0 -100, fade 0.5, scale 1, noTransition ] [ renderTile ctx tile ]


renderConnectedCell : GCtx -> GI -> Cell -> Svg msg
renderConnectedCell ctx gi (Cell tile) =
    group [ moveToGI ctx gi, scale 0.8 ] [ renderTile ctx tile ]


renderLeavingCell : GCtx -> GI -> Cell -> Svg msg
renderLeavingCell ctx _ (Cell tile) =
    group
        [ move 0 -400
        , scale 0.2
        , fade 0
        , transition "all 500ms ease-in"
        , transition "all 500ms cubic-bezier(.84,.32,.72,.11)"
        , transition "all 500ms cubic-bezier(1,.1,.75,1.30)"
        ]
        [ renderTile ctx tile ]


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
