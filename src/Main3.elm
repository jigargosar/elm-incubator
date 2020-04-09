module Main3 exposing (main)

import Browser
import Css
    exposing
        ( displayFlex
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
                            IdleCell fallingToIdx

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
                            EnteringStartCell idx

                        False ->
                            ResetIdleCell idx
            in
            viewGridCells (List.map func gridCellIndices)

        GeneratedFalling _ generated ->
            let
                func idx =
                    case List.member idx generated of
                        True ->
                            EnteringCell idx

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
    | EnteringStartCell Idx
    | EnteringCell Idx
    | ResetIdleCell Idx


viewCell : CellView -> HM
viewCell cellView =
    case cellView of
        IdleCell idx ->
            viewIdleCell idx

        ConnectedCell idx ->
            viewConnectedCell idx

        LeavingCell idx ->
            viewLeavingCell idx

        EnteringStartCell idx ->
            viewEnteringStartCell idx

        EnteringCell idx ->
            viewEnteringCell idx

        ResetIdleCell idx ->
            viewResetIdleCell idx


viewEnteringStartCell : Int -> HM
viewEnteringStartCell idx =
    viewStyledWaterCellAt
        idx
        [ opacity (num 0)
        , transforms [ translateY (px -300) ]
        , transitionNone
        ]


viewResetIdleCell : Int -> HM
viewResetIdleCell idx =
    viewStyledWaterCellAt idx [ transitionNone ]


viewEnteringCell : Int -> HM
viewEnteringCell idx =
    viewStyledWaterCellAt idx [ transforms [ translateY zero ] ]


viewLeavingCell : Int -> HM
viewLeavingCell idx =
    viewStyledWaterCellAt idx
        [ left (pct 50)
        , top (px 0)
        , opacity (num 0)
        , transforms [ Css.scale 0.5 ]
        ]


viewIdleCell : Int -> HM
viewIdleCell idx =
    viewStyledWaterCellAt idx []


viewConnectedCell : Int -> HM
viewConnectedCell idx =
    viewStyledWaterCellAt idx [ transforms [ Css.scale 0.5 ] ]


viewStyledWaterCellAt : Int -> List Css.Style -> HM
viewStyledWaterCellAt idx styles =
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
            :: transitionDefault
            :: styles
        )
        []
        [ text (String.fromInt idx) ]


defaultTransitionDuration =
    500


bgc =
    Css.property "background-color"


transitionNone =
    Css.property "transition" "none"


transitionDefault =
    Css.property "transition" ("all " ++ String.fromFloat defaultTransitionDuration ++ "ms")



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.Styled.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
