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
                    |> move -100 -100
                , ellipse "red" 100 100
                    |> move -100 -100
                ]
        ]


rectangle : String -> Float -> Float -> S
rectangle c w h =
    R w h |> S c itx


ellipse c w h =
    E w h |> S c itx


group : List S -> S
group ss =
    G ss |> S "none" itx


move : Float -> Float -> S -> S
move dx dy =
    mapTX <| \(TX x y) -> TX (x + dx) (y + dy)


mapTX : (TX -> TX) -> S -> S
mapTX fn (S c tx f) =
    S c (fn tx) f


type F
    = R Float Float
    | E Float Float
    | G (List S)


type S
    = S String TX F


type TX
    = TX Float Float


itx : TX
itx =
    TX 0 0


moveTX : Float -> Float -> TX -> TX
moveTX dx dy (TX x y) =
    TX (x + dx) (y + dy)


draw : S -> HM
draw (S c (TX dx dy) s) =
    case s of
        R w h ->
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

        E w h ->
            Svg.ellipse
                [ Px.rx w
                , Px.ry h
                , TA.transform [ TT.Translate dx dy ]
                , SA.fill c
                ]
                []

        G ss ->
            Svg.g
                [ TA.transform [ TT.Translate dx dy ]
                , SA.fill c
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
