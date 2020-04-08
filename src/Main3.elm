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
        , vh
        , width
        )
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (div, styled, text)
import List.Extra
import Process
import Task



-- Model


type Model
    = Idle
    | Connecting Int (List Int)
    | Collecting (List Int)
    | Falling (List Int) (List ( Int, Int ))
    | ResetBeforeGenerating (List Int)


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Idle
    , delayedSequence
        ( seconds 1, Set <| Connecting 9 [ 10, 11, 12, 13, 20, 25, 26, 27 ] )
        [ ( seconds 1, Set <| Collecting <| 9 :: [ 10, 11, 12, 13, 20, 25, 26, 27 ] )
        , ( seconds 1
          , Set <|
                Falling (9 :: [ 10, 11, 12, 13, 20, 25, 26, 27 ])
                    [ ( 2, 9 )
                    , ( 3, 10 )
                    , ( 18, 25 )
                    , ( 4, 18 )
                    , ( 5, 19 )
                    , ( 19, 26 )
                    , ( 6, 27 )
                    ]
          )
        ]
    )


seconds n =
    n * 1000


delayedSequence : ( Float, Msg ) -> List ( Float, Msg ) -> Cmd Msg
delayedSequence ( millis, msg ) rest =
    Process.sleep millis |> Task.perform (\_ -> OnTimeout msg rest)



-- Update


type Msg
    = NoOp
    | Set Model
    | OnTimeout Msg (List ( Float, Msg ))


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Set m2 ->
            ( m2, Cmd.none )

        OnTimeout msg [] ->
            update msg model

        OnTimeout msg (next :: rest) ->
            update msg model
                |> addCmd (delayedSequence next rest)


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

        Connecting last previousIndices ->
            styled div
                [ gridStyle gridRows gridColumns gridCellWidth ]
                []
                (List.range 1 (gridRows * gridColumns)
                    |> List.map (viewCell (last :: previousIndices))
                )

        Collecting ls ->
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

        ResetBeforeGenerating empty ->
            styled div
                [ gridStyle gridRows gridColumns gridCellWidth ]
                []
                (List.range 1 (gridRows * gridColumns)
                    |> List.map (viewEmptyCellNoTransition empty)
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


viewEmptyCellNoTransition : List Int -> Int -> HM
viewEmptyCellNoTransition emptyLs idx =
    case List.member idx emptyLs of
        True ->
            viewWaterCell idx
                [ opacity (num 0)
                , transition []
                ]

        False ->
            viewWaterCell idx
                [ opacity (num 1)
                , transition []
                ]


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


viewWaterCell idx styles =
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
        []
        []
        [ styled div
            (bgc "dodgerblue"
                :: width (px gridCellWidth)
                :: height (px gridCellWidth)
                :: left (px x)
                :: top (px y)
                :: position fixed
                :: opacity (num 1)
                :: transition
                    [ Transitions.transform 200
                    , Transitions.opacity 200
                    , Transitions.top 200
                    , Transitions.left 200
                    ]
                :: styles
            )
            []
            [ text (String.fromInt idx) ]
        ]


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
