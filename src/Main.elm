module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Html exposing (Html)
import Svg
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


type F
    = R Float Float


view : Model -> Html Msg
view _ =
    let
        sw =
            600

        sh =
            600
    in
    Svg.svg [ TA.viewBox (sw * -0.5) (sh * -0.5) sw sh ]
        [ draw (R 400 400)
        ]


type alias HM =
    Html Msg


draw : F -> HM
draw s =
    case s of
        R w h ->
            Svg.rect [ Px.width w, Px.height h, TA.transform [ TT.Translate (w * -0.5) (h * -0.5) ] ] []


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
