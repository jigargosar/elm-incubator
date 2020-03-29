module Main exposing (main)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (autofocus, class)



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



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


view : Model -> Html Msg
view _ =
    div [ class "measure-wide center" ]
        [ div
            [ class "pv2 ph3"
            , class "ba br-pill b--moon-gray "
            , class "fw-b--transparent fw-shadow-1"
            , class "flex"
            ]
            [ input
                [ class "bg-transparent bn outline-0"
                , class "lh-title flex-auto"
                , autofocus True
                ]
                []
            ]
        ]
