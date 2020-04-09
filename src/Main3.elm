module Main3 exposing (main)

import Browser
import Css
    exposing
        ( batch
        , displayFlex
        , fixed
        , height
        , left
        , num
        , opacity
        , pct
        , position
        , px
        , top
        , transforms
        , translateY
        , vh
        , width
        , zero
        )
import Css.Transitions as Transitions exposing (transition)
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
                            300
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
                                    0
                            in
                            ( EndingDrag (GeneratedStart duration genIndices)
                            , delay duration StepEndingDrag
                            )

                        GeneratedStart _ genIndices ->
                            let
                                duration =
                                    300
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
    styled div
        [ displayFlex
        , Css.alignItems Css.center
        , Css.justifyContent Css.center
        , Css.minHeight (vh 100)
        ]
        []
        [ viewGrid m ]


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


viewGrid : Model -> HM
viewGrid m =
    case m of
        Idle ->
            styled div
                [ gridStyle gridRows gridColumns gridCellWidth ]
                []
                (List.range 1 (gridRows * gridColumns)
                    |> List.map (viewCell [])
                )

        Dragging connected ->
            styled div
                [ gridStyle gridRows gridColumns gridCellWidth ]
                []
                (List.range 1 (gridRows * gridColumns)
                    |> List.map (viewCell connected)
                )

        EndingDrag endingDragState ->
            case endingDragState of
                LeavingAndFalling _ leaving falling _ ->
                    styled div
                        [ gridStyle gridRows gridColumns gridCellWidth ]
                        []
                        (List.range 1 (gridRows * gridColumns)
                            |> List.map (viewLeavingAndFallingCell leaving falling)
                        )

                GeneratedStart _ genLs ->
                    styled div
                        [ gridStyle gridRows gridColumns gridCellWidth ]
                        []
                        (List.range 1 (gridRows * gridColumns)
                            |> List.map (viewGeneratedCellsStart genLs)
                        )

                GeneratedFalling _ genLs ->
                    styled div
                        [ gridStyle gridRows gridColumns gridCellWidth ]
                        []
                        (List.range 1 (gridRows * gridColumns)
                            |> List.map (viewFallingGeneratedCells genLs)
                        )


gridStyle : Int -> Int -> Float -> Css.Style
gridStyle r c w =
    batch
        [ Css.property "display" "grid"
        , Css.property "grid-template-columns" <|
            "repeat("
                ++ String.fromInt c
                ++ ", "
                ++ String.fromFloat w
                ++ "px)"
        , Css.property "grid-template-rows" <|
            "repeat("
                ++ String.fromInt r
                ++ ", "
                ++ String.fromFloat gridCellWidth
                ++ "px)"
        , Css.property "grid-gap" "1px"
        ]


viewGeneratedCellsStart : List Int -> Int -> HM
viewGeneratedCellsStart genLs idx =
    case List.member idx genLs of
        True ->
            viewWaterCell2 True
                idx
                [ opacity (num 0)
                , transforms [ translateY (px -300) ]
                ]

        False ->
            viewWaterCell2 True idx []


viewFallingGeneratedCells : List Int -> Int -> HM
viewFallingGeneratedCells genLs idx =
    case List.member idx genLs of
        True ->
            viewWaterCell idx
                [ transforms [ translateY zero ]
                ]

        False ->
            viewWaterCell idx []


viewLeavingAndFallingCell : List Int -> List ( Int, Int ) -> Int -> HM
viewLeavingAndFallingCell leavingLs fallingLs idx =
    case List.Extra.find (Tuple.first >> (==) idx) fallingLs of
        Just ( _, dstIdx ) ->
            viewWaterCell dstIdx []

        Nothing ->
            case List.member idx leavingLs of
                True ->
                    viewWaterCell idx
                        [ left (pct 50)
                        , top (px 0)
                        , opacity (num 0)
                        , transforms [ Css.scale 0.5 ]
                        ]

                False ->
                    viewWaterCell idx []


viewCell : List Int -> Int -> HM
viewCell connectedIndices idx =
    if List.member idx connectedIndices then
        viewWaterCell idx [ transforms [ Css.scale 0.5 ] ]

    else
        viewWaterCell idx []


viewWaterCell =
    viewWaterCell2 False


viewWaterCell2 resetTransitions idx styles =
    let
        xi =
            modBy gridColumns (idx - 1)

        yi =
            (idx - 1) // gridColumns

        x =
            (toFloat xi * (gridCellWidth + 1)) + 300

        y =
            (toFloat yi * (gridCellWidth + 1)) + 400
    in
    styled div
        (bgc "dodgerblue"
            :: width (px gridCellWidth)
            :: height (px gridCellWidth)
            :: left (px x)
            :: top (px y)
            :: position fixed
            :: opacity (num 1)
            :: transition
                (if resetTransitions then
                    []

                 else
                    [ Transitions.transform 200
                    , Transitions.opacity 200
                    , Transitions.top 200
                    , Transitions.left 200
                    ]
                )
            :: styles
        )
        []
        [ text (String.fromInt idx) ]


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
