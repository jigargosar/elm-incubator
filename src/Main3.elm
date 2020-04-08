module Main3 exposing (main)

import Basics.Extra exposing (uncurry)
import Browser
import Css exposing (backgroundColor, batch, displayFlex, flexFlow2, height, hex, num, pct, px, row, transforms, vh, width, wrap)
import Html.Styled exposing (div, styled)
import Process
import Task



-- Model


type Model
    = Idle
    | Connecting Int (List Int)


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Idle
    , batchDelayed
        [ ( 1 * 1000, SetConnected 6 [ 7, 8, 9, 14, 19, 18, 17 ] )
        ]
    )


batchDelayed list =
    Cmd.batch (List.map (uncurry delay) list)


delay : Float -> msg -> Cmd msg
delay afterMillis msg =
    Process.sleep afterMillis
        |> Task.perform (\_ -> msg)



-- Update


type Msg
    = NoOp
    | SetConnected Int (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        SetConnected last previousIndices ->
            ( Connecting last previousIndices, Cmd.none )


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
    5


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


viewCell : List Int -> Int -> HM
viewCell connectedIndices idx =
    if List.member idx connectedIndices then
        styled div [ bgc "dodgerblue", transforms [ Css.scale 0.5 ] ] [] []

    else
        styled div [ bgc "dodgerblue" ] [] []


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
