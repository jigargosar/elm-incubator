module Main exposing (main)

import AABB exposing (AABB)
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


type alias Camera =
    { origin : Float2
    }


initialCamera : Camera
initialCamera =
    { origin = ( 0, 0 ) }


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



-- Player


type alias Player =
    { loc : Location }


initialPlayer : Player
initialPlayer =
    { loc = Loc.fromTuple ( 10, 10 )
    }



-- Model


type alias Model =
    { gm : GridMap
    , player : Player
    , camera : Camera
    , screenSize : Float2
    , seed : Seed
    }


type alias Flags =
    { now : Int, window : { width : Float, height : Float } }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initialSeed =
            Random.initialSeed (flags.now |> always 4)

        window =
            flags.window
    in
    ( { gm = initialGM
      , player = initialPlayer
      , screenSize = pairFloat window.width window.height
      , camera = initialCamera
      , seed = initialSeed
      }
        |> focusPlayer
    , Cmd.none
    )


focusPlayer : Model -> Model
focusPlayer model =
    { model | camera = focusLoc model.screenSize model.gm model.player.loc model.camera }


focusLoc : Float2 -> GridMap -> Location -> Camera -> Camera
focusLoc ss gm loc cam =
    let
        locCord =
            gmToWorldCords gm loc

        focusCord =
            gmBounds gm
                |> AABB.shrink ss
                |> AABB.grow (Tuple.scale 2 gm.cellSize)
                |> AABB.clampPoint locCord
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

        KeyDown key ->
            let
                player =
                    model.player

                gm =
                    model.gm

                dxy =
                    keyToXY key

                ml =
                    Dimension.validLocation
                        (player.loc |> Loc.shift dxy)
                        gm.dimension

                nPlayer =
                    case ml of
                        Nothing ->
                            player

                        Just loc ->
                            { player | loc = loc }
            in
            ( { model | player = nPlayer }
                |> focusPlayer
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


keyToXY string =
    case string of
        "ArrowLeft" ->
            ( -1, 0 )

        "ArrowRight" ->
            ( 1, 0 )

        "ArrowUp" ->
            ( 0, -1 )

        "ArrowDown" ->
            ( 0, 1 )

        _ ->
            ( 0, 0 )


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
                , viewPlayer model.gm model.player
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


viewPlayer gm player =
    Svg.g [ S.transforms [ S.translate (gmToWorldCords gm player.loc) ] ]
        [ circle (Tuple.first gm.cellSize)
            [ S.fill "dodgerblue"
            , S.transforms [ S.scale 0.5 ]
            , S.fade 0.8
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


gmBounds : GridMap -> AABB
gmBounds gm =
    gmSize gm
        |> AABB.fromSize
        |> AABB.shift gm.offset


gmRightBottom : GridMap -> Float2
gmRightBottom gm =
    gmSize gm
        |> Tuple.scale 0.5
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
                [ viewIf True <| rect gm.cellSize [ S.strokeWhite, S.fade 0.5 ]
                , words (loc |> Loc.toTuple |> Debug.toString)
                    [ S.fillWhite
                    , S.textAnchorStart
                    , S.dominantBaselineHanging
                    , S.transforms
                        [ S.translate (gm.cellSize |> Tuple.scale -0.5)
                        , S.scale 0.9
                        , S.translate ( 2, 2 )
                        ]
                    , S.fade 0.5
                    ]
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



--square : Float -> List (Svg.Attribute msg) -> Svg.Svg msg
--square w =
--    rect (Tuple.repeat w)


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
