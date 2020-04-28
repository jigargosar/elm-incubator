module Main exposing (main)

import Basics.Extra exposing (atMost)
import Browser exposing (Document)
import Dict exposing (Dict)
import Dict.Extra
import Html exposing (Html, text)
import Random
import Svg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx
import TypedSvg.Types exposing (Opacity(..))



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

        points =
            randomPointsIn size
    in
    Svg.svg [ TypedSvg.Attributes.viewBox 0 0 size.width size.height ]
        [ Svg.g [] []

        --, points |> renderPoints |> Svg.g []
        , renderFrequencyDict (points |> pointsToFrequencyDict) |> Svg.g []
        ]


pointsToFrequencyDict : List Point -> Dict ( Float, Float ) Int
pointsToFrequencyDict =
    List.map pointToTuple >> Dict.Extra.frequencies


renderFrequencyDict =
    Dict.toList >> List.map (\( ( x, y ), freq ) -> renderDot x y (toFloat freq * 0.1 |> atMost 1))


renderPoints =
    List.map (\{ x, y } -> renderDot x y 0.1)


randomPointsIn : Size -> List Point
randomPointsIn size =
    Random.step (randomWalkerPointsGenerator size 50000) (Random.initialSeed 3)
        |> Tuple.first


randomWalkerPointsGenerator : Size -> Int -> Random.Generator (List Point)
randomWalkerPointsGenerator size len =
    let
        func2 _ ( ( h, t ), seed0 ) =
            Random.step (nextPointGenerator size h) seed0
                |> Tuple.mapFirst (\np -> ( np, h :: t ))

        func startPoint seed =
            List.range 0 (len - 1)
                |> List.foldl func2 ( ( startPoint, [] ), seed )
                |> Tuple.first
                |> consToList
                |> List.reverse
    in
    Random.map2 func
        (randomPointGenerator size)
        Random.independentSeed


consToList ( h, t ) =
    h :: t


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


renderDot x y o =
    let
        dotWidth =
            1
    in
    Svg.rect
        [ TypedSvg.Attributes.InPx.x x
        , TypedSvg.Attributes.InPx.y y
        , TypedSvg.Attributes.InPx.width dotWidth
        , TypedSvg.Attributes.InPx.height dotWidth
        , TypedSvg.Attributes.opacity (Opacity o)
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


pointToTuple point =
    ( point.x, point.y )


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
    Random.map2
        (\x y ->
            newPoint
                ((x * (size.width - 1))
                    |> roundFloat
                )
                ((y * (size.height - 1))
                    |> roundFloat
                )
        )
        random
        random


roundFloat : Float -> Float
roundFloat =
    round >> toFloat



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
