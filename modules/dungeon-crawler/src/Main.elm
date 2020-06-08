module Main exposing (main)

import Basics.More exposing (..)
import Browser
import Browser.Events
import Dict exposing (Dict)
import Dimension exposing (Dimension)
import Html exposing (Html, div, text)
import Json.Decode as JD
import Location as Loc
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
    div [ S.noSelect ]
        [ Svg.svg
            [ S.width sw
            , S.height sh
            , class "fixed left-0 top-0"
            , SA.viewBox ([ sw / -2, sh / -2, sw, sh ] |> spacedFloats)
            , S.noFill
            ]
            [ rect model.screenSize [ S.fillBlackA 0.6 ]
            , circle 100 [ S.fillBlack ]
            , square 100 [ S.fillWhite, S.rx100 ]
            , viewGridMap
            ]
        , div [ class "relative pv3 f3 white tc" ] [ text "Dungeon Crawler" ]
        ]


type alias GridMap =
    { dimension : Dimension
    , cellSize : Float2
    , origin : Float2
    , dict : Dict Int2 Int
    }


viewGridMap =
    let
        gm =
            { dimension = Dimension.new 3 4
            , cellSize = twice 64
            , origin = twice 0
            , dict = Dict.empty
            }

        toWorldCords =
            Loc.toFloat >> Tuple.scaleBoth gm.cellSize >> Tuple.add gm.origin

        viewLocation loc =
            Svg.g [ S.transforms [ S.translate (toWorldCords loc) ] ]
                [ rect gm.cellSize [ S.fillBlackA 0.1 ]
                , words (Debug.toString loc) [ S.strokeWhite ]
                ]
    in
    Svg.g []
        (gm.dimension
            |> Dimension.toLocations
            |> List.map viewLocation
        )


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


words : String -> List (Svg.Attribute msg) -> Svg.Svg msg
words string xs =
    Svg.text_
        (SA.textAnchor "middle"
            :: SA.dominantBaseline "central"
            :: xs
        )
        [ Svg.text string ]



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
