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
    | Leaving (List Int)
    | Falling (List Int) (List ( Int, Int ))
    | GeneratedStart (List Int)
    | GeneratedFalling (List Int)


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Idle
    , loopSimulation
    )


loopSimulation : Cmd Msg
loopSimulation =
    simulate (simulation ++ [ ( seconds 2, LoopSimulation ) ])


simulation : List ( number, Msg )
simulation =
    let
        connected =
            [ 9, 10, 11, 12, 13, 20, 27, 26, 25 ]

        dragSim =
            DragStart 9
                :: List.map DragOver connected
                |> List.map (Tuple.pair 100)
    in
    []
        ++ dragSim
        --++ [ ( seconds 1, Set <| Dragging connected ) ]
        ++ [ ( seconds 1, DragEnd ) ]
        --++ [ ( seconds 1, Set <| Leaving connected )]
        ++ [ ( 0
             , Set (Falling connected (computeChanges connected []))
             )
           , ( seconds 1, Set <| GeneratedStart [ 2, 3, 4, 5, 6, 11, 12, 13, 20 ] )
           , ( 0, Set <| GeneratedFalling [ 2, 3, 4, 5, 6, 11, 12, 13, 20 ] )
           , ( seconds 1, Set Idle )
           ]


computeChanges : List number -> List ( number, number ) -> List ( number, number )
computeChanges emptyIndices changes =
    case List.sortBy negate emptyIndices of
        [] ->
            changes

        firstEmpty :: remainingEmpty ->
            case firstNonEmptyIndexAbove firstEmpty remainingEmpty of
                Just neIdx ->
                    computeChanges (neIdx :: remainingEmpty) (( neIdx, firstEmpty ) :: changes)

                Nothing ->
                    computeChanges remainingEmpty changes


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



-- Update


type Msg
    = NoOp
    | Set Model
    | DragStart Int
    | DragOver Int
    | DragEnd
    | OnTimeout Msg (List ( Float, Msg ))
    | LoopSimulation


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Set m2 ->
            ( m2, Cmd.none )

        OnTimeout msg pending ->
            update msg model
                |> addCmd (simulate pending)

        LoopSimulation ->
            ( model, loopSimulation )

        DragStart unverifiedIdx ->
            case ( model, validIdx unverifiedIdx ) of
                ( Idle, Just idx ) ->
                    ( Dragging [ idx ], Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DragOver unverifiedIdx ->
            case ( model, validIdx unverifiedIdx ) of
                ( Dragging draggingIndices, Just idx ) ->
                    if List.member idx draggingIndices then
                        ( model, Cmd.none )

                    else
                        ( Dragging (idx :: draggingIndices), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DragEnd ->
            case model of
                Dragging draggingIndices ->
                    ( Leaving draggingIndices, Cmd.none )

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

        Leaving ls ->
            styled div
                [ gridStyle gridRows gridColumns gridCellWidth ]
                []
                (List.range 1 (gridRows * gridColumns)
                    |> List.map (viewLeavingCell ls)
                )

        Falling leaving falling ->
            styled div
                [ gridStyle gridRows gridColumns gridCellWidth ]
                []
                (List.range 1 (gridRows * gridColumns)
                    |> List.map (viewFallingCell leaving falling)
                )

        GeneratedStart genLs ->
            styled div
                [ gridStyle gridRows gridColumns gridCellWidth ]
                []
                (List.range 1 (gridRows * gridColumns)
                    |> List.map (viewGeneratedCellsStart genLs)
                )

        GeneratedFalling genLs ->
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


viewFallingCell : List Int -> List ( Int, Int ) -> Int -> HM
viewFallingCell leavingLs fallingLs idx =
    case List.Extra.find (Tuple.first >> (==) idx) fallingLs of
        Just ( _, dstIdx ) ->
            viewWaterCell dstIdx []

        Nothing ->
            viewLeavingCell leavingLs idx


viewLeavingCell : List Int -> Int -> HM
viewLeavingCell ls idx =
    case List.member idx ls of
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
