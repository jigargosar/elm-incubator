module GameRunner exposing (main)

import AbstractGame
import Browser exposing (Document)
import Html exposing (Html, text)



-- Model


type Model
    = Model AbstractGame.GameModel


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( Model AbstractGame.initGame
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
        [ text (Debug.toString g) ]



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
