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
        [ draw <|
            group
                [ rectangle "dodgerblue" 200 200
                ]
        ]


move : Float -> Float -> S -> S
move dx dy (S (TX x y) f) =
    S (TX (x + dx) (y + dy)) f


group : List S -> S
group ss =
    G ss |> S (TX 0 0)


rectangle : String -> Float -> Float -> S
rectangle c w h =
    R c w h |> S (TX 0 0)


type F
    = R String Float Float
    | E String Float Float
    | G (List S)


type S
    = S TX F


type TX
    = TX Float Float


draw : S -> HM
draw (S (TX dx dy) s) =
    case s of
        R c w h ->
            Svg.rect
                [ Px.width w
                , Px.height h
                , TA.transform
                    [ TT.Translate (w * -0.5) (h * -0.5)
                    , TT.Translate dx dy
                    ]
                , SA.fill c
                ]
                []

        E c w h ->
            Svg.ellipse
                [ Px.width w
                , Px.height h
                , TA.transform [ TT.Translate dx dy ]
                , SA.fill c
                ]
                []

        G ss ->
            Svg.g
                [ TA.transform [ TT.Translate dx dy ]
                ]
                (List.map draw ss)



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
