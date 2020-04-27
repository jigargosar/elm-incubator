module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, text)
import List.Extra
import Random
import Random.Extra
import Random.List
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


randomPointsIn : Size -> List Point
randomPointsIn size =
    Random.step (bar size 4000) (Random.initialSeed 0)
        |> Tuple.first


bar : Size -> Int -> Random.Generator (List Point)
bar size max =
    randomPointGenerator size
        |> Random.andThen
            (\start -> foo size max start [])


foo : Size -> Int -> Point -> List Point -> Random.Generator (List Point)
foo size max h t =
    if max == 0 then
        Random.constant (List.reverse (h :: t))

    else
        nextPointGenerator size h
            |> Random.andThen (\nh -> Random.lazy (\_ -> foo size (max - 1) nh (h :: t)))


nextPointGenerator : Size -> Point -> Random.Generator Point
nextPointGenerator size point =
    Random.map2
        (\func by ->
            func by point
                |> constrainPointInSize size
        )
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


randomPointGenerator : Size -> Random.Generator Point
randomPointGenerator size =
    Random.map2 (\x y -> newPoint (x * (size.width - 1)) (y * (size.height - 1))) random random



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
