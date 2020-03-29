module Main exposing (main)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (autofocus, class, value)



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


type Model
    = Model SearchInput


type SearchInput
    = SI String Bool


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Model (SI "" False)
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | OnSIFocusChange Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        OnSIFocusChange isFocused ->
            ( case model of
                Model (SI v _) ->
                    Model (SI v isFocused)
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


view : Model -> Html Msg
view _ =
    div [ class "pt4 measure-wide center" ]
        [ inputPill [ autofocus True, value "hello world!" ]
        , div [ class "pv3" ] []
        , inputPill []
        ]


inputPill ias =
    div
        [ class "pv2 ph3"
        , class "ba br-pill b--moon-gray "
        , class "fw-b--transparent fw-shadow-1"
        , class "flex"
        ]
        [ input
            ([ class "bg-transparent bn outline-0"
             , class "lh-title flex-auto"
             ]
                ++ ias
            )
            []
        ]
