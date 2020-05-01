module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)



-- Model


type alias Model =
    {}


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
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


type alias DM =
    Document Msg


view : Model -> DM
view _ =
    Document "2048"
        [ div [ class "f3 pa3" ] [ text "2048 grid" ]
        , div [ class "pl5 pv3 " ] [ viewGrid ]
        ]


viewGrid =
    let
        rows =
            [ [ 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0 ]
            ]

        viewRow row =
            div [ class "flex" ] (List.map viewCell row)

        viewCell num =
            div [ class "w3 tc" ] [ text (String.fromInt num) ]
    in
    div [ class "flex flex-column code f1" ] (List.map viewRow rows)



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
