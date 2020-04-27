module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, text)
import Random
import Svg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx



-- Model


type alias Model =
    {}


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
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


type alias DM =
    Document Msg


view : Model -> DM
view _ =
    Document "Main"
        [ text "Hello Main"
        , viewRW
        ]


viewRW =
    let
        size =
            newSize 300 300
    in
    Svg.svg [ TypedSvg.Attributes.viewBox 0 0 size.width size.height ]
        [ randomPointsIn size
            |> List.map renderDotAtPoint
            |> Svg.g []
        ]


randomPointsIn size =
    let
        _ =
            Random.float 0 1
    in
    [ newPoint 10 10
    , newPoint 11 11
    ]


nextPointGenerator : Point -> Random.Generator Point
nextPointGenerator point =
    Random.map2 (\func by -> func by point)
        (Random.uniform mapX [ mapY ])
        (Random.uniform inc [ dec ])



-- Dot


renderDotAtPoint { x, y } =
    renderDot x y


renderDot x y =
    let
        dotWidth =
            1
    in
    Svg.rect
        [ TypedSvg.Attributes.InPx.x x
        , TypedSvg.Attributes.InPx.y y
        , TypedSvg.Attributes.InPx.width dotWidth
        , TypedSvg.Attributes.InPx.height dotWidth
        ]
        []



-- Random


random : Random.Generator Float
random =
    Random.float 0 1



-- Basics


inc =
    add 1


dec =
    add -1


add =
    (+)



-- Point


type alias Point =
    { x : Float, y : Float }


newPoint : Float -> Float -> Point
newPoint x y =
    { x = x, y = y }


mapX : (Float -> Float) -> Point -> Point
mapX func point =
    { point | x = func point.x }


mapY : (Float -> Float) -> Point -> Point
mapY func point =
    { point | y = func point.y }


constrainPointInSize : Size -> Point -> Point
constrainPointInSize size point =
    Point (clamp 0 (size.width - 1) point.x) (clamp 0 (size.height - 1) point.y)


randomPoint : Random.Generator Point
randomPoint =
    Random.map2 newPoint random random



-- Size


type alias Size =
    { width : Float, height : Float }


newSize : Float -> Float -> Size
newSize w h =
    Size w h



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
