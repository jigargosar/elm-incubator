module GameRunner exposing (main)

import AbstractGame as G
import Browser exposing (Document)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)



-- Model


type Model
    = Model G.GameModel


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( Model G.initGame
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
view (Model g) =
    Document "GameRunner"
        [ div [ class "pa3" ]
            [ text (Debug.toString (G.info g))
            ]
        ]



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
