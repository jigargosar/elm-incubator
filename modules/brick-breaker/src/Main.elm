module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, text)
import Svg
import Svg.Attributes
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx



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
    let
        w =
            600

        h =
            300
    in
    Document "Brick Breaker"
        [ Svg.svg
            [ TypedSvg.Attributes.viewBox 0 0 w h
            , Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "gray"
            , Svg.Attributes.strokeWidth "1"
            ]
            [ Svg.g
                []
                [ rect (w / 2) (h - 60) 80 20 ]
            ]
        ]


rect x y w h =
    Svg.rect
        [ TypedSvg.Attributes.InPx.x (x - w / 2)
        , TypedSvg.Attributes.InPx.y (y - h / 2)
        , TypedSvg.Attributes.InPx.width w
        , TypedSvg.Attributes.InPx.height h
        ]
        []



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
