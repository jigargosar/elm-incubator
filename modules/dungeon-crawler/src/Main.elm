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
    , gm : GridMap
    , screenSize : Float2
    , seed : Seed
    }


initialGM : GridMap
initialGM =
    { dimension = Dimension.new 3 4
    , cellSize = twice 64
    , dict = Dict.empty
    , offset = Tuple.zero
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
      , gm = initialGM
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
                loc =
                    screenToWorld model.screenSize event.offsetPos
                        |> gmFromWorldCords model.gm
                        |> Debug.log "debug"
            in
            ( model, Cmd.none )


screenToWorld ss p =
    ss
        |> Tuple.scale -0.5
        |> Tuple.add p


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
            , viewGridMap model.gm
            , words "Dungeon Crawler"
                [ S.dominantBaselineHanging
                , S.fillWhite
                , class "f3"
                , S.transforms [ S.translateY (sh * -0.5 + 16) ]
                ]
            ]
        , div [ class "relative", S.noEvents ]
            [ div [ class "pv3 f3 white tc dn" ] [ text "Dungeon Crawler" ]
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
gmToWorldCords gm loc =
    loc
        |> Loc.toFloat
        |> Tuple.mul gm.cellSize
        |> Tuple.add (gm.cellSize |> Tuple.halve)
        |> Tuple.add (gmSize gm |> Tuple.halve |> Tuple.negate)
        |> Tuple.add gm.offset


gmFromWorldCords gm wc =
    wc
        |> Tuple.add (Tuple.negate gm.offset)
        |> Tuple.add (Tuple.add (gm.cellSize |> Tuple.halve |> Tuple.negate) (gmSize gm |> Tuple.halve))
        |> Tuple.mul (Tuple.invert gm.cellSize)
        |> Tuple.map round


gmGet : GridMap -> Location -> Maybe Int
gmGet gm loc =
    if Dimension.isValidLocation loc gm.dimension then
        get (Loc.toTuple loc) gm.dict
            |> Maybe.withDefault 0
            |> Just

    else
        Nothing


viewGridMap gm =
    let
        toWorldCords =
            gmToWorldCords gm

        dataAt : Location -> String
        dataAt =
            gmGet gm >> Maybe.map fromInt >> Maybe.withDefault "invalid loc"

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
