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
        , scale
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
                      --|> always Cmd.none
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



--styled div
--    [ displayFlex
--    , Css.alignItems Css.center
--    , Css.justifyContent Css.center
--    , Css.minHeight (vh 100)
--    ]
--    []
--    [ viewGrid m ]


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
            viewGridCells (List.map IdleCell gridCellIndices)

        Dragging connected ->
            let
                func idx =
                    if List.member idx connected then
                        ConnectedCell idx

                    else
                        IdleCell idx
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
                            FallingCell idx fallingToIdx

                        Nothing ->
                            if List.member idx leaving then
                                LeavingCell idx

                            else
                                IdleCell idx
            in
            viewGridCells (List.map func gridCellIndices)

        GeneratedStart _ generated ->
            let
                func idx =
                    case List.member idx generated of
                        True ->
                            StartEnteringCell idx

                        False ->
                            ResetIdleCell idx
            in
            viewGridCells (List.map func gridCellIndices)

        GeneratedFalling _ generated ->
            let
                func idx =
                    case List.member idx generated of
                        True ->
                            IdleCell idx

                        False ->
                            ResetIdleCell idx
            in
            viewGridCells (List.map func gridCellIndices)


type alias Idx =
    Int


type CellView
    = IdleCell Idx
    | ConnectedCell Idx
    | LeavingCell Idx
    | FallingCell Idx Idx
    | StartEnteringCell Idx
    | ResetIdleCell Idx


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



--translateAddress : Address -> Css.Transform {}
--translateAddress address =
--    let
--        ( x, y ) =
--            addressToXY address
--    in
--    Css.translate2 (px x) (px y)


movePoint : number -> number -> ( number, number ) -> ( number, number )
movePoint dx dy ( x, y ) =
    ( x + dx, y + dy )


type TRANSITION
    = FAST
    | MEDIUM
    | INSTANT


type Form
    = Rectangle Float Float


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
        Rectangle w h ->
            styled div
                (opacity (num s.fade)
                    :: transforms [ translate2 (px s.x) (px s.y), scale s.scale ]
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
                []


rectangle : Float -> Float -> Shape
rectangle w h =
    initShape (Rectangle w h)


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



--move : Float -> Float -> Shape -> Shape
--move dx dy (Shape s) =
--    Shape { s | x = s.x + dx, y = s.y + dy }


moveTo : Float -> Float -> Shape -> Shape
moveTo x y (Shape s) =
    Shape { s | x = x, y = y }


sca : Float -> Shape -> Shape
sca sc (Shape s) =
    Shape { s | scale = s.scale * sc }


fade o (Shape s) =
    Shape { s | fade = o }


fill : String -> Shape -> Shape
fill c (Shape s) =
    Shape { s | fill = c }


waterRect : Shape
waterRect =
    rectangle gridCellWidth gridCellWidth
        |> fill "dodgerblue"
        |> setT MEDIUM


moveToAddr : Address -> Shape -> Shape
moveToAddr address =
    let
        ( x, y ) =
            addressToXY address
    in
    moveTo x y


setT : TRANSITION -> Shape -> Shape
setT t (Shape s) =
    Shape { s | transition = t }


viewCell : CellView -> HM
viewCell cellView =
    let
        viewHelp idx styles =
            styled div styles [] [ text (String.fromInt idx) ]

        viewHelp2 idx fn =
            fn waterRect |> drawSH
    in
    case cellView of
        IdleCell idx ->
            --viewHelp idx
            --    [ waterCellStyle (AtGridIndex idx) 1 1
            --    , transitionDefault
            --    ]
            viewHelp2 idx
                (moveToAddr (AtGridIndex idx))

        ConnectedCell idx ->
            --viewHelp idx
            --    [ waterCellStyle (AtGridIndex idx) 0.5 1
            --    , transitionFast
            --    ]
            viewHelp2 idx
                (moveToAddr (AtGridIndex idx)
                    >> setT FAST
                    >> sca 0.5
                )

        LeavingCell idx ->
            --viewHelp idx
            --    [ waterCellStyle AtWaterCollector 0.5 0
            --    , transitionDefault
            --    ]
            viewHelp2 idx
                (moveToAddr AtWaterCollector
                    >> sca 0.5
                    >> fade 0
                )

        FallingCell fromIdx toIdx ->
            --viewHelp fromIdx
            --    [ waterCellStyle (AtGridIndex toIdx) 1 1
            --    , transitionDefault
            --    ]
            viewHelp2 fromIdx
                (moveToAddr (AtGridIndex toIdx))

        StartEnteringCell idx ->
            --viewHelp idx
            --    [ waterCellStyle (AtGridIndexEntrance idx) 0 0
            --    , transitionNone
            --    ]
            viewHelp2 idx
                (moveToAddr (AtGridIndexEntrance idx)
                    >> sca 0
                    >> fade 0
                    >> setT INSTANT
                )

        ResetIdleCell idx ->
            --viewHelp idx
            --    [ waterCellStyle (AtGridIndex idx) 1 1
            --    , transitionNone
            --    ]
            viewHelp2 idx
                (moveToAddr (AtGridIndex idx)
                    >> setT INSTANT
                )



--waterCellStyle : Address -> Float -> Float -> Css.Style
--waterCellStyle address scaleV fadeV =
--    Css.batch
--        [ cellStyle address scaleV fadeV
--        , bgc "dodgerblue"
--        ]
--cellStyle : Address -> Float -> Float -> Css.Style
--cellStyle address scaleV fadeV =
--    Css.batch
--        [ transforms [ translateAddress address, scale scaleV ]
--        , opacity (num fadeV)
--        , position fixed
--        , width (px gridCellWidth)
--        , height (px gridCellWidth)
--        ]


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



--transitionNone =
--    Css.property "transition" "none"
--
--
--transitionDefault =
--    Css.property "transition" ("all " ++ String.fromFloat defaultTransitionDuration ++ "ms")
--
--
--transitionFast =
--    Css.property "transition" ("all " ++ String.fromFloat fastTransitionDuration ++ "ms")
-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.Styled.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
