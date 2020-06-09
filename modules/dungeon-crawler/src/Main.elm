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
import Tuple exposing (first)
import Tuple.More as Tuple



-- Model


type alias Model =
    { dimension : Dimension
    , gm : GridMap
    , camera : Camera
    , screenSize : Float2
    , seed : Seed
    }


type alias Camera =
    { origin : Float2
    }


initialCamera : Camera
initialCamera =
    { origin = ( 0, 32 ) }


camFocus : Float2 -> Camera -> Camera
camFocus wp camera =
    { camera | origin = wp }


type alias GridMap =
    { dimension : Dimension
    , cellSize : Float2
    , dict : Dict Int2 Int
    , offset : Float2
    }


initialGM : GridMap
initialGM =
    { dimension = Dimension.new 32 32
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
      , camera = initialCamera
      , seed = initialSeed
      }
        |> postInit
    , Cmd.none
    )


postInit : Model -> Model
postInit model =
    { model | camera = focusLoc model.screenSize model.gm Loc.zero model.camera }


focusLoc : Float2 -> GridMap -> Location -> Camera -> Camera
focusLoc ss gm loc cam =
    let
        locCord =
            gmToWorldCords gm loc

        leftTop =
            gmLeftTop gm

        minCamCord =
            Tuple.add leftTop (Tuple.halve ss)
                |> Tuple.add (Tuple.negate gm.cellSize)

        focusCord =
            locCord
                |> Tuple.map2 atLeast minCamCord
    in
    camFocus focusCord cam



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
                maybeLoc =
                    screenToWorld model.screenSize model.camera event.offsetPos
                        |> gmFromWorldCords model.gm
                        |> Debug.log "debug"
            in
            ( case maybeLoc of
                Just loc ->
                    { model | gm = gmOnLocClick loc model.gm }

                Nothing ->
                    model
            , Cmd.none
            )


screenToWorld : Float2 -> Camera -> Float2 -> Float2
screenToWorld ss cam p =
    ss
        |> Tuple.scale -0.5
        |> Tuple.add cam.origin
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
            , Svg.g []
                [ circle 128 [ S.fillWhite, S.strokeWidth 32, S.strokeBlack ]
                ]
            , Svg.g [ S.transforms [ S.translate (Tuple.negate model.camera.origin) ] ]
                [ viewGridMap model.gm
                ]
            , Svg.g []
                [ words "Dungeon Crawler"
                    [ S.dominantBaselineHanging
                    , S.fillWhite
                    , class "f3"
                    , S.transforms [ S.translateY (sh * -0.5 + 16) ]
                    ]
                ]
            ]
        , div [ class "relative", S.noEvents ]
            [ div [ class "pv3 f3 white tc dn" ] [ text "Dungeon Crawler" ]
            ]
        ]


gmSize gm =
    Dimension.toFloat gm.dimension
        |> Tuple.mul gm.cellSize


gmLeftTop : GridMap -> Float2
gmLeftTop gm =
    gmSize gm
        |> Tuple.scale -0.5
        |> Tuple.add gm.offset


gmZerothCellCenter gm =
    let
        leftTop =
            gm
                |> gmLeftTop

        halfCellSize =
            gm.cellSize
                |> Tuple.halve
    in
    Tuple.add leftTop halfCellSize


gmToWorldCords : GridMap -> Location -> Float2
gmToWorldCords gm loc =
    let
        zCenter =
            gm
                |> gmZerothCellCenter

        locOffset =
            loc
                |> Loc.toFloat
                |> Tuple.mul gm.cellSize
    in
    Tuple.add zCenter locOffset


gmFromWorldCords : GridMap -> Float2 -> Maybe Location
gmFromWorldCords gm wc =
    let
        loc =
            Tuple.sub wc (gmZerothCellCenter gm)
                |> Tuple.mul (Tuple.invert gm.cellSize)
                |> Tuple.map round
                |> Loc.fromTuple
    in
    Dimension.validLocation loc gm.dimension


gmGet : GridMap -> Location -> Maybe Int
gmGet gm loc =
    if Dimension.isValidLocation loc gm.dimension then
        get (Loc.toTuple loc) gm.dict
            |> Maybe.withDefault 0
            |> Just

    else
        Nothing


gmOnLocClick loc gm =
    let
        locTuple =
            Loc.toTuple loc

        fn mx =
            case mx of
                Just 10 ->
                    Nothing

                Nothing ->
                    Just 10

                _ ->
                    Nothing
    in
    { gm | dict = gm.dict |> Dict.update locTuple fn }


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
                , words (Debug.toString loc) [ S.fillWhite, S.transforms [ S.translateY (gm.cellSize |> first |> mul -0.25) ] ]
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
