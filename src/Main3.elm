module Main3 exposing (main)

import Browser
import Css
    exposing
        ( fixed
        , height
        , num
        , opacity
        , position
        , px
        , transforms
        , translate2
        , width
        )
import Html.Styled exposing (div, styled, text)
import List.Extra
import Process
import Task



-- Model


type Model
    = Idle
    | Dragging (List Int)
    | EndingDrag EndingDragState


type EndingDragState
    = LeavingAndFalling Float (List Int) (List ( Int, Int )) (List Int)
    | GeneratedStart Float (List Int)
    | GeneratedFalling Float (List Int)


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Idle
    , loopSimulation
    )


loopSimulation : Cmd Msg
loopSimulation =
    simulate (simulation ++ [ ( seconds 2, OnLoopSimulation ) ])


simulation : List ( number, Msg )
simulation =
    let
        connected =
            [ 9, 10, 11, 12, 13, 20, 27, 26, 25 ]

        dragSim =
            OnDragStart 9
                :: List.map OnDragOver connected
                |> List.map (Tuple.pair 100)
    in
    []
        ++ dragSim
        ++ [ ( 300, OnDragEnd ) ]


computeFallingFromEmptyIndices : List number -> ( List ( number, number ), List number ) -> ( List ( number, number ), List number )
computeFallingFromEmptyIndices emptyIndices ( changes, newEmptyIndices ) =
    case List.sortBy negate emptyIndices of
        [] ->
            ( changes, newEmptyIndices |> Debug.log "debug" )

        firstEmpty :: remainingEmpty ->
            case firstNonEmptyIndexAbove firstEmpty remainingEmpty of
                Just neIdx ->
                    computeFallingFromEmptyIndices (neIdx :: remainingEmpty)
                        ( ( neIdx, firstEmpty ) :: changes, newEmptyIndices )

                Nothing ->
                    computeFallingFromEmptyIndices remainingEmpty ( changes, firstEmpty :: newEmptyIndices )


firstNonEmptyIndexAbove idx emptyIndices =
    case idxAbove idx of
        Nothing ->
            Nothing

        Just above ->
            if List.member above emptyIndices then
                firstNonEmptyIndexAbove above emptyIndices

            else
                Just above


idxAbove idx =
    validIdx (idx - gridColumns)


seconds n =
    n * 1000


simulate : List ( Float, Msg ) -> Cmd Msg
simulate list =
    case list of
        [] ->
            Cmd.none

        ( millis, msg ) :: rest ->
            Process.sleep millis |> Task.perform (\_ -> OnTimeout msg rest)


delay : Float -> b -> Cmd b
delay millis msg =
    Process.sleep millis |> Task.perform (\_ -> msg)



-- Update


type Msg
    = NoOp
    | OnDragStart Int
    | OnDragOver Int
    | OnDragEnd
    | StepEndingDrag
    | OnTimeout Msg (List ( Float, Msg ))
    | OnLoopSimulation


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        OnTimeout msg pending ->
            update msg model
                |> addCmd (simulate pending)

        OnLoopSimulation ->
            ( model, loopSimulation )

        OnDragStart unverifiedIdx ->
            case ( model, validIdx unverifiedIdx ) of
                ( Idle, Just idx ) ->
                    ( Dragging [ idx ], Cmd.none )

                _ ->
                    ( model, Cmd.none )

        OnDragOver unverifiedIdx ->
            case ( model, validIdx unverifiedIdx ) of
                ( Dragging draggingIndices, Just idx ) ->
                    if List.member idx draggingIndices then
                        ( model, Cmd.none )

                    else
                        ( Dragging (idx :: draggingIndices), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        OnDragEnd ->
            case model of
                Dragging draggingIndices ->
                    let
                        ( changes, newEmptyIndices ) =
                            computeFallingFromEmptyIndices draggingIndices ( [], [] )

                        duration =
                            defaultTransitionDuration
                    in
                    ( EndingDrag (LeavingAndFalling duration draggingIndices changes newEmptyIndices)
                    , delay duration StepEndingDrag
                    )

                _ ->
                    ( model, Cmd.none )

        StepEndingDrag ->
            case model of
                EndingDrag endingDragState ->
                    case endingDragState of
                        LeavingAndFalling _ _ _ genIndices ->
                            let
                                duration =
                                    defaultTransitionDuration
                            in
                            ( EndingDrag (GeneratedStart duration genIndices)
                            , delay defaultTransitionDuration StepEndingDrag
                            )

                        GeneratedStart _ genIndices ->
                            let
                                duration =
                                    defaultTransitionDuration
                            in
                            ( EndingDrag (GeneratedFalling duration genIndices), delay duration StepEndingDrag )

                        GeneratedFalling _ _ ->
                            ( Idle, Cmd.none )

                _ ->
                    ( model, Cmd.none )


addCmd : Cmd msg -> ( a, Cmd msg ) -> ( a, Cmd msg )
addCmd c2 ( m, c1 ) =
    ( m, Cmd.batch [ c1, c2 ] )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


type alias HM =
    Html.Styled.Html Msg


view : Model -> HM
view m =
    viewGrid m


gridColumns =
    7


gridRows =
    5


minIdx =
    1


maxIdx =
    gridRows * gridColumns


validIdx idx =
    if clamp minIdx maxIdx idx == idx then
        Just idx

    else
        Nothing


gridCellWidth =
    50


gridCellIndices : List Int
gridCellIndices =
    List.range 1 (gridRows * gridColumns)


viewGrid : Model -> HM
viewGrid m =
    case m of
        Idle ->
            viewGridCells (List.map (\idx -> CellView idx IdleCell) gridCellIndices)

        Dragging connected ->
            let
                func idx =
                    if List.member idx connected then
                        CellView idx ConnectedCell

                    else
                        CellView idx IdleCell
            in
            viewGridCells (List.map func gridCellIndices)

        EndingDrag endingDragState ->
            viewEndingDragGrid endingDragState


viewGridCells cellViewList =
    styled div
        []
        []
        (List.map viewCell cellViewList)


viewEndingDragGrid : EndingDragState -> HM
viewEndingDragGrid endingDragState =
    case endingDragState of
        LeavingAndFalling _ leaving falling _ ->
            let
                func idx =
                    case List.Extra.find (Tuple.first >> (==) idx) falling of
                        Just ( _, fallingToIdx ) ->
                            CellView fallingToIdx <| FallingCell idx

                        Nothing ->
                            if List.member idx leaving then
                                CellView idx LeavingCell

                            else
                                CellView idx IdleCell
            in
            viewGridCells (List.map func gridCellIndices)

        GeneratedStart _ generated ->
            let
                func idx =
                    case List.member idx generated of
                        True ->
                            CellView idx StartEnteringCell

                        False ->
                            CellView idx ResetIdleCell
            in
            viewGridCells (List.map func gridCellIndices)

        GeneratedFalling _ generated ->
            let
                func idx =
                    case List.member idx generated of
                        True ->
                            CellView idx IdleCell

                        False ->
                            CellView idx ResetIdleCell
            in
            viewGridCells (List.map func gridCellIndices)


type alias Idx =
    Int


type CellViewState
    = IdleCell
    | ConnectedCell
    | LeavingCell
    | FallingCell Idx
    | StartEnteringCell
    | ResetIdleCell


type CellView
    = CellView Idx CellViewState


type Address
    = AtGridIndex Idx
    | AtGridIndexEntrance Idx
    | AtWaterCollector


addressToXY : Address -> ( Float, Float )
addressToXY address =
    case address of
        AtGridIndex idx ->
            gridIndexToPoint idx

        AtGridIndexEntrance idx ->
            gridIndexToPoint idx
                |> movePoint 0 -300

        AtWaterCollector ->
            ( gridXOffset + (gridWidth / 2), gridCellWidth )


movePoint : number -> number -> ( number, number ) -> ( number, number )
movePoint dx dy ( x, y ) =
    ( x + dx, y + dy )


type TRANSITION
    = FAST
    | MEDIUM
    | INSTANT


type Form
    = Rectangle Float Float String


type Shape
    = Shape ShapeRecord


type alias ShapeRecord =
    { fade : Float
    , x : Float
    , y : Float
    , scale : Float
    , fill : String
    , stroke : String
    , strokeWidth : Float
    , styles : List Css.Style
    , transition : TRANSITION
    , form : Form
    }


drawSH (Shape s) =
    case s.form of
        Rectangle w h content ->
            styled div
                (opacity (num s.fade)
                    :: transforms [ translate2 (px s.x) (px s.y), Css.scale s.scale ]
                    :: bgc s.fill
                    :: Css.property "transition"
                        ("all "
                            ++ String.fromFloat
                                (case s.transition of
                                    INSTANT ->
                                        0

                                    FAST ->
                                        fastTransitionDuration

                                    MEDIUM ->
                                        defaultTransitionDuration
                                )
                            ++ "ms"
                        )
                    :: position fixed
                    :: width (px w)
                    :: height (px h)
                    :: s.styles
                )
                []
                [ text content ]


initShape : Form -> Shape
initShape form =
    Shape
        { fade = 1
        , x = 0
        , y = 0
        , scale = 1
        , fill = "none"
        , stroke = "none"
        , strokeWidth = 1
        , styles = []
        , transition = INSTANT
        , form = form
        }



--noinspection ElmUnusedSymbol


move : Float -> Float -> Shape -> Shape
move dx dy (Shape s) =
    Shape { s | x = s.x + dx, y = s.y + dy }


moveTo : Float -> Float -> Shape -> Shape
moveTo x y (Shape s) =
    Shape { s | x = x, y = y }


scale : Float -> Shape -> Shape
scale sc (Shape s) =
    Shape { s | scale = s.scale * sc }


fade : Float -> Shape -> Shape
fade o (Shape s) =
    Shape { s | fade = o }


fill : String -> Shape -> Shape
fill c (Shape s) =
    Shape { s | fill = c }


waterRect : Int -> Shape
waterRect idx =
    initShape (Rectangle gridCellWidth gridCellWidth (String.fromInt idx))
        |> fill "dodgerblue"
        |> trans MEDIUM


moveToAddr_ : Address -> Shape -> Shape
moveToAddr_ address =
    let
        ( x, y ) =
            addressToXY address
    in
    moveTo x y


moveAtGridIdx =
    AtGridIndex >> moveToAddr_


moveAtGridIdxEntrance =
    AtGridIndexEntrance >> moveToAddr_


moveAtWaterCollector =
    moveToAddr_ AtWaterCollector


trans : TRANSITION -> Shape -> Shape
trans t (Shape s) =
    Shape { s | transition = t }


viewCell : CellView -> HM
viewCell (CellView idx cellViewState) =
    let
        drawWaterRect fn =
            fn (waterRect idx) |> drawSH
    in
    case cellViewState of
        IdleCell ->
            drawWaterRect (moveAtGridIdx idx)

        ConnectedCell ->
            drawWaterRect
                (moveAtGridIdx idx
                    >> trans FAST
                    >> scale 0.5
                )

        LeavingCell ->
            drawWaterRect
                (moveAtWaterCollector
                    >> scale 0.5
                    >> fade 0
                )

        FallingCell fromIdx ->
            waterRect fromIdx
                |> moveAtGridIdx idx
                |> drawSH

        StartEnteringCell ->
            drawWaterRect
                (moveAtGridIdxEntrance idx
                    >> scale 0
                    >> fade 0
                    >> trans INSTANT
                )

        ResetIdleCell ->
            drawWaterRect
                (moveAtGridIdx idx
                    >> trans INSTANT
                )


gridIndexToPoint : Int -> ( Float, Float )
gridIndexToPoint idx =
    let
        xi =
            modBy gridColumns (idx - 1)

        yi =
            (idx - 1) // gridColumns

        x =
            (toFloat xi * (gridCellWidth + 1)) + gridXOffset

        y =
            (toFloat yi * (gridCellWidth + 1)) + gridYOffset
    in
    ( x, y )


gridXOffset =
    300


gridYOffset =
    400


gridWidth =
    toFloat gridColumns * (gridCellWidth + 1)


defaultTransitionDuration =
    1000


fastTransitionDuration =
    200


bgc =
    Css.property "background-color"



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.Styled.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
