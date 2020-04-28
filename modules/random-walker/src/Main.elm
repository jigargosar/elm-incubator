module Main exposing (main)

import Basics.Extra exposing (atMost)
import Browser exposing (Document)
import Browser.Events
import Dict exposing (Dict)
import Dict.Extra
import Html exposing (Html, text)
import Random
import Svg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx
import TypedSvg.Types exposing (Opacity(..))



-- Model


type alias FreqDict =
    Dict ( Float, Float ) Int


freqDictInsertPoint : Point -> FreqDict -> FreqDict
freqDictInsertPoint point =
    Dict.update (pointToTuple point) (Maybe.map inc >> Maybe.withDefault 1 >> Just)


freqDictSingleton : Point -> FreqDict
freqDictSingleton point =
    freqDictInsertPoint point Dict.empty


type alias Model =
    { freqDict : FreqDict
    , last : Point
    , size : Size
    , seed : Random.Seed
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    let
        size =
            newSize 200 200

        start =
            size
                |> sizeMidPoint
                |> roundPoint
    in
    ( { freqDict = freqDictSingleton start
      , last = start
      , size = size
      , seed = Random.initialSeed 0
      }
    , Cmd.none
    )


updateRandomWalker : Model -> Model
updateRandomWalker model =
    let
        ( nextPoint, nextSeed ) =
            Random.step (nextPointGenerator model.size model.last) model.seed
    in
    { model
        | seed = nextSeed
        , last = nextPoint
        , freqDict = freqDictInsertPoint nextPoint model.freqDict
    }



-- Update


type Msg
    = NoOp
    | Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Tick ->
            ( updateRandomWalker model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame (always Tick)
        ]



-- View


type alias DM =
    Document Msg


view : Model -> DM
view model =
    --let
    --    points =
    --        Random.step (randomWalkerPointsGenerator model.size 50000) (Random.initialSeed 1)
    --            |> Tuple.first
    --
    --    freqDict =
    --        points |> pointsToFrequencyDict
    --in
    Document "Main"
        [ text "Hello Main"
        , viewRandomWalker model.size model.freqDict
        ]


viewRandomWalker size freqDict =
    Svg.svg [ TypedSvg.Attributes.viewBox 0 0 size.width size.height ]
        [ Svg.g [] []
        , renderFrequencyDict freqDict |> Svg.g []
        ]


pointsToFrequencyDict : List Point -> FreqDict
pointsToFrequencyDict =
    List.map pointToTuple >> Dict.Extra.frequencies


renderFrequencyDict =
    Dict.toList >> List.map (\( ( x, y ), freq ) -> renderDot x y (toFloat freq * 0.05 |> atMost 1))


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


roundPoint : Point -> Point
roundPoint =
    mapX roundFloat >> mapY roundFloat


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


sizeMidPoint : Size -> Point
sizeMidPoint size =
    newPoint (size.width / 2) (size.height / 2)



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
