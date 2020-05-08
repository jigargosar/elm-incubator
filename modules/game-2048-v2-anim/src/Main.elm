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
    Document "2048 Animated"
        [ div [ class "pa3 measure-center" ]
            [ div [ class "f3" ] [ text "Play 2048" ]
            , viewGrid initialGrid
            ]
        ]


initialGrid =
    [ [ 0, 0, 0, 0 ]
    , [ 0, 2, 0, 0 ]
    , [ 0, 0, 4, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


viewGrid =
    List.map viewRow
        >> div [ class "code f4" ]


viewRow =
    List.map viewCell
        >> div [ class "" ]


viewCell num =
    div [] [ text (String.fromInt num) ]



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
