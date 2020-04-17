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


type SeedsGrid
    = SeedsGrid (Grid Cell) GridState


type GridState
    = GridIdle
    | GridConnecting GI (List GI)
    | GridCollecting CollectingState


type alias CollectingState =
    { leaving : Cons GI
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


startConnecting : GI -> Grid Cell -> Maybe SeedsGrid
startConnecting =
    let
        initConnecting : GI -> Grid Cell -> SeedsGrid
        initConnecting gi grid =
            GridConnecting gi []
                |> SeedsGrid grid

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
    = StartConnecting GI
    | ToggleConnecting GI
    | StartCollecting


type Return
    = SetGridState GridState
    | SetSeedsGrid SeedsGrid
    | Stay


customUpdate : Msg -> Model -> Return
customUpdate message (Model _ (SeedsGrid grid gs)) =
    case message of
        StartConnecting gi ->
            case gs of
                GridIdle ->
                    case startConnecting gi grid of
                        Just ng ->
                            SetSeedsGrid ng

                        Nothing ->
                            Stay

                _ ->
                    Stay

        ToggleConnecting gi ->
            case gs of
                GridIdle ->
                    case startConnecting gi grid of
                        Just ng ->
                            SetSeedsGrid ng

                        Nothing ->
                            Stay

                GridConnecting lastGI remaining ->
                    if areConnectable gi lastGI grid && not (List.member gi remaining) then
                        SetGridState
                            (GridConnecting gi (lastGI :: remaining))

                    else if List.head remaining == Just gi then
                        SetGridState
                            (GridConnecting gi (List.drop 1 remaining))

                    else
                        Stay

                _ ->
                    Stay

        StartCollecting ->
            case gs of
                GridConnecting lastGI ((_ :: _) as list) ->
                    SetGridState
                        (GridCollecting (CollectingState (Cons.cons lastGI list) []))

                _ ->
                    Stay


update : Msg -> Model -> ( Model, Cmd Msg )
update message ((Model _ (SeedsGrid grid gs)) as model) =
    case message of
        StartConnecting gi ->
            case gs of
                GridIdle ->
                    case startConnecting gi grid of
                        Just ng ->
                            ( setGridState ng model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleConnecting gi ->
            case gs of
                GridIdle ->
                    case startConnecting gi grid of
                        Just ng ->
                            ( setGridState ng model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                GridConnecting lastGI remaining ->
                    if areConnectable gi lastGI grid && not (List.member gi remaining) then
                        ( setGridState
                            (GridConnecting gi (lastGI :: remaining)
                                |> SeedsGrid grid
                            )
                            model
                        , Cmd.none
                        )

                    else if List.head remaining == Just gi then
                        ( setGridState
                            (GridConnecting gi (List.drop 1 remaining)
                                |> SeedsGrid grid
                            )
                            model
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StartCollecting ->
            case gs of
                GridConnecting lastGI ((_ :: _) as list) ->
                    ( setGridState
                        (GridCollecting (CollectingState (Cons.cons lastGI list) [])
                            |> SeedsGrid grid
                        )
                        model
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


setGridState : SeedsGrid -> Model -> Model
setGridState gs (Model win _) =
    Model win gs



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

        GridConnecting gi giList ->
            let
                ciCons =
                    Cons.cons gi giList
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


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
