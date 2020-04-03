module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Html exposing (Html)
import Svg
import Svg.Attributes as SA
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types as TT



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
    Html Msg


view : Model -> Html Msg
view _ =
    let
        sw =
            600

        sh =
            600
    in
    Svg.svg [ TA.viewBox (sw * -0.5) (sh * -0.5) sw sh ]
        [ draw (S "dodgerblue" (R 400 400))
        ]


type F
    = R Float Float


type S
    = S String F


draw : S -> HM
draw (S c s) =
    case s of
        R w h ->
            Svg.rect
                [ Px.width w
                , Px.height h
                , TA.transform [ TT.Translate (w * -0.5) (h * -0.5) ]
                , SA.fill c
                ]
                []



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
