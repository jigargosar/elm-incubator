module Main exposing (main)

import Basics.More exposing (..)
import Browser
import Browser.Events
import Dict exposing (Dict)
import Dimension exposing (Dimension)
import Html exposing (Html, div, text)
import Html.Events.Extra.Mouse as ME
import Json.Decode as JD
import Location as Loc exposing (Location)
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
    | SvgClick ME.Event


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

        SvgClick event ->
            let
                _ =
                    Debug.log "event" event
            in
            ( model, Cmd.none )


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
            , ME.onClick SvgClick
            ]
            [ rect model.screenSize [ S.fillBlackA 0.6 ]
            , circle 128 [ S.fillWhite, S.strokeWidth 32, S.strokeBlack ]
            , viewGridMap
            , words "Dungeon Crawler" [ S.fillWhite, class "f3" ]
            ]
        , div [ class "relative", S.noEvents ]
            [ div [ class "pv3 f3 white tc" ]
                [ text "Dungeon Crawler" ]
            ]
        ]


type alias GridMap =
    { dimension : Dimension
    , cellSize : Float2
    , dict : Dict Int2 Int
    , offset : Float2
    }


gmSize gm =
    Dimension.toFloat gm.dimension
        |> Tuple.mul gm.cellSize


gmToWorldCords : GridMap -> Location -> Float2
gmToWorldCords gm =
    let
        leftTop =
            gm
                |> gmSize
                |> Tuple.scale -0.5

        zeroThCellCenter =
            leftTop
                |> Tuple.add (gm.cellSize |> Tuple.halve)
    in
    Loc.toFloat >> Tuple.mul gm.cellSize >> Tuple.add zeroThCellCenter >> Tuple.add gm.offset


viewGridMap =
    let
        gm : GridMap
        gm =
            { dimension = Dimension.new 3 4
            , cellSize = twice 64
            , dict = Dict.empty
            , offset = Tuple.zero
            }

        toWorldCords =
            gmToWorldCords gm

        dataAt : Location -> String
        dataAt loc =
            gm.dict
                |> getOr 0 (Loc.toTuple loc)
                |> fromInt

        viewLocationDebug loc =
            Svg.g [ S.transforms [ S.translate (toWorldCords loc) ] ]
                [ rect gm.cellSize [ S.strokeWhite ]
                , words (dataAt loc) [ S.fillWhite ]
                ]
    in
    Svg.g []
        [ Svg.g []
            [ rect (gmSize gm) [ S.fill "green", S.fade 0.5 ] ]
        , Svg.g []
            (gm.dimension
                |> Dimension.toLocations
                |> List.map viewLocationDebug
            )
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


words : String -> List (Svg.Attribute msg) -> Svg.Svg msg
words string xs =
    Svg.text_
        (S.textAnchorMiddle
            :: S.dominantBaselineCentral
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
