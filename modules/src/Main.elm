module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href)



-- Model


type alias Model =
    { modules : List String }


type alias Flags =
    { modules : List String }


init : Flags -> ( Model, Cmd Msg )
init f =
    ( Model f.modules
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


type alias DM =
    Document Msg


view : Model -> DM
view model =
    Document "Elm Incubator"
        [ div [ class "f4 pv2" ] [ text "Elm Incubator Modules" ]
        , div [] (List.map vm model.modules)
        ]


vm string =
    div [] [ a [ href string ] [ text string ] ]



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
