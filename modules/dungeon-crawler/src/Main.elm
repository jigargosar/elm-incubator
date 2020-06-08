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
import Svg.Attributes as A



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
            , S.noFill
            ]
            [ Svg.rect [ class "w-100 h-100", S.fillBlackA 0.2 ] []
            , Svg.rect [ S.width 100, S.height 100, S.fillWhite, S.rx100 ] []
            , Svg.circle [ S.r 50, S.fillBlack ] []
            ]
        , div [ class "pv3 f3 relative" ] [ text "Dungeon Crawler" ]
        ]


rect ( w, h ) xs =
    Svg.rect
        (S.x (w * 0.5)
            :: S.y (h * 0.5)
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
