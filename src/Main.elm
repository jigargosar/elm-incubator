module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Html exposing (Html)
import Svg
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as Px



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
    Svg.svg [ TA.viewBox 0 0 600 600 ]
        [ Svg.rect [ Px.x 0, Px.y 0, Px.width 100, Px.height 100 ] []
        ]


empty : Html msg
empty =
    Html.text ""



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
