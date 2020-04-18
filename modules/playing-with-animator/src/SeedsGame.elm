module SeedsGame exposing (main)

import Basics.Extra exposing (uncurry)
import Browser exposing (Document)
import Cons exposing (Cons)
import Draw exposing (canvas, circle, fade, group, move, rect, scale, square)
import Grid exposing (GI, Grid)
import List.Extra
import Maybe.Extra
import Process
import Svg exposing (Svg)
import Task



-- SEED GRID


type SeedsGrid
    = SeedsGrid (Grid Cell) GridState


type GridState
    = GridIdle
    | GridConnecting ConnectingState
    | GridCollecting CollectingState


type ConnectingState
    = ConnectingState GI (List GI)


extendConnection : GI -> Grid Cell -> ConnectingState -> Maybe ConnectingState
extendConnection gi grid (ConnectingState lastGI remaining) =
    if areConnectable gi lastGI grid && not (List.member gi remaining) then
        Just (ConnectingState gi (lastGI :: remaining))

    else
        Nothing


startConnection : GI -> Grid Cell -> Maybe ConnectingState
startConnection gi grid =
    case canStartConnectionAt gi grid of
        True ->
            Just (ConnectingState gi [])

        False ->
            Nothing


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


type alias CollectingState =
    { leaving : List GI
    , falling : List ( GI, GI )
    }


type Cell
    = Cell Tile


type Tile
    = Water
    | Wall


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


areCellsAtIndicesConnectable : GI -> GI -> Grid Cell -> Bool
areCellsAtIndicesConnectable ia ib grid =
    Maybe.map2 (==) (Grid.get ia grid) (Grid.get ib grid)
        |> Maybe.withDefault False


areConnectable : GI -> GI -> Grid Cell -> Bool
areConnectable aa bb grid =
    areCellsAtIndicesConnectable aa bb grid
        && isAdjacentTo aa bb


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
    = ToggleConnecting GI
    | StartCollecting


type Return
    = SetGridState GridState
    | Stay


gotoGridState : GridState -> Return
gotoGridState =
    SetGridState


gotoConnecting : ConnectingState -> Return
gotoConnecting cs =
    gotoGridState (GridConnecting cs)


maybeGoto : (a -> Return) -> Maybe a -> Return
maybeGoto func maybeVal =
    case maybeVal of
        Nothing ->
            Stay

        Just val ->
            func val


maybeGotoConnecting : Maybe ConnectingState -> Return
maybeGotoConnecting =
    maybeGoto gotoConnecting


gotoCollecting : CollectingState -> Return
gotoCollecting cs =
    gotoGridState (GridCollecting cs)


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
                GridConnecting (ConnectingState lastGI ((_ :: _) as list)) ->
                    let
                        leaving =
                            lastGI :: list

                        falling =
                            computeFalling leaving grid []
                    in
                    gotoCollecting { leaving = leaving, falling = falling }

                _ ->
                    Stay


computeFalling : List GI -> Grid Cell -> List ( GI, GI ) -> List ( GI, GI )
computeFalling emptyIndices grid falling =
    case List.sort emptyIndices |> List.reverse of
        [] ->
            falling

        destIdx :: remainingEmpty ->
            case firstMovableCellIdxAbove destIdx remainingEmpty grid of
                Nothing ->
                    computeFalling
                        remainingEmpty
                        grid
                        falling

                Just srcIdx ->
                    computeFalling
                        (srcIdx :: remainingEmpty)
                        grid
                        (( srcIdx, destIdx ) :: falling)


firstMovableCellIdxAbove : GI -> List GI -> Grid Cell -> Maybe GI
firstMovableCellIdxAbove gi____ emptyIndices grid =
    let
        gi =
            giUp gi____
    in
    case Grid.get gi grid of
        Just cell ->
            if
                not (List.member gi emptyIndices)
                    && isCellMovable cell
            then
                Just gi

            else
                firstMovableCellIdxAbove gi emptyIndices grid

        Nothing ->
            Nothing


isCellMovable : Cell -> Bool
isCellMovable (Cell s) =
    case s of
        Water ->
            True

        Wall ->
            False


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        return =
            customUpdate message model
    in
    case return of
        SetGridState gridState ->
            ( setGridState gridState model, Cmd.none )

        Stay ->
            ( model, Cmd.none )


setSeedsGrid : SeedsGrid -> Model -> Model
setSeedsGrid gs (Model win _) =
    Model win gs


setGridState : GridState -> Model -> Model
setGridState gridState ((Model _ (SeedsGrid grid _)) as model) =
    setSeedsGrid (SeedsGrid grid gridState) model



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
view (Model window gs) =
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


renderGrid : SeedsGrid -> Svg msg
renderGrid (SeedsGrid grid gs) =
    case gs of
        GridIdle ->
            gridToListWithCtx renderIdleCell grid
                |> group []

        GridConnecting (ConnectingState gi giList) ->
            let
                ciCons =
                    Cons.cons gi giList
            in
            gridToListWithCtx (renderConnectingCell ciCons) grid
                |> group []

        GridCollecting { leaving, falling } ->
            let
                renderCell ctx gi =
                    let
                        maybeFallingToIdx =
                            List.Extra.find (Tuple.first >> (==) gi) falling
                                |> Maybe.map Tuple.second
                    in
                    case maybeFallingToIdx of
                        Just to ->
                            renderIdleCell ctx to

                        Nothing ->
                            if List.member gi leaving then
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
