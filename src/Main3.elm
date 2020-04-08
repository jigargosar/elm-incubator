module Main3 exposing (..)

import Browser
import Css exposing (backgroundColor, displayFlex, flexFlow2, height, hex, num, px, row, vh, width, wrap)
import Html.Styled exposing (div, styled)



-- Model


type alias Model =
    {}


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( {}
    , Cmd.none
    )



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


type alias HM =
    Html.Styled.Html Msg


view : Model -> HM
view _ =
    styled div
        [ displayFlex
        , Css.alignItems Css.center
        , Css.justifyContent Css.center
        , Css.minHeight (vh 100)
        ]
        []
        [ viewGrid ]


gridColumns =
    5


gridRows =
    5


gridCellWidth =
    50


viewGrid =
    styled div
        [ Css.property "display" "grid"
        , Css.property "grid-template-columns" <|
            "repeat("
                ++ String.fromInt gridColumns
                ++ ", "
                ++ String.fromFloat gridCellWidth
                ++ "px)"
        , Css.property "grid-template-rows" <|
            "repeat("
                ++ String.fromInt gridRows
                ++ ", "
                ++ String.fromFloat gridCellWidth
                ++ "px)"
        , Css.property "grid-gap" "1px"
        ]
        []
        (List.range 1 (5 * 5) |> List.map viewCell2)


viewCell2 : a -> HM
viewCell2 _ =
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
