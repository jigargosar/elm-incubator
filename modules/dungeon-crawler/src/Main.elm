module Main exposing (main)

import Basics.More exposing (..)
import Browser
import Browser.Events
import Dimension exposing (Dimension)
import Html exposing (Html, div, text)
import Json.Decode as JD
import Random exposing (Generator, Seed)
import Styles as S
import Svg
import Svg.Attributes as SA
import Tuple.More as Tuple



-- Model


type alias Model =
    { dimension : Dimension
    , screenSize : Float2
    , seed : Seed
    }


type alias Flags =
    { now : Int, window : { width : Float, height : Float } }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        dimension =
            Dimension.new 12 16

        initialSeed =
            Random.initialSeed (flags.now |> always 4)

        window =
            flags.window
    in
    ( { dimension = dimension
      , screenSize = pairFloat window.width window.height
      , seed = initialSeed
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | KeyDown String
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        KeyDown _ ->
            ( model
            , Cmd.none
            )

        Tick _ ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown
            (JD.field "key" JD.string
                |> JD.map KeyDown
            )
        , subscribeIf False (Browser.Events.onAnimationFrameDelta Tick)
        ]



-- View


type alias HM =
    Html Msg


view : Model -> Html Msg
view model =
    let
        ( sw, sh ) =
            model.screenSize
    in
    div []
        [ Svg.svg
            [ S.width sw
            , S.height sh
            , class "fixed left-0 top-0"
            , SA.viewBox ([ sw / -2, sh / -2, sw, sh ] |> spacedFloats)
            , S.noFill
            ]
            [ rect model.screenSize [ class "w-100 h-100", S.fillBlackA 0.2 ]
            , square 100 [ S.fillWhite, S.rx100 ]
            , circle 50 [ S.fillBlack ]
            ]
        , div [ class "pv3 f3 relative" ] [ text "Dungeon Crawler" ]
        ]


circle : Float -> List (Svg.Attribute msg) -> Svg.Svg msg
circle r xs =
    Svg.circle (S.r r :: xs) []


square : Float -> List (Svg.Attribute msg) -> Svg.Svg msg
square w =
    rect (Tuple.repeat w)


rect : Float2 -> List (Svg.Attribute msg) -> Svg.Svg msg
rect ( w, h ) xs =
    Svg.rect
        (S.x (w / -2)
            :: S.y (h / -2)
            :: S.width w
            :: S.height h
            :: xs
        )
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
