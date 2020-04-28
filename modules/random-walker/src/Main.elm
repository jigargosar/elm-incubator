module Main exposing (main)

import Basics.Extra exposing (atMost)
import Browser exposing (Document)
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Random exposing (Generator)
import Svg
import Svg.Attributes
import Svg.Keyed
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx



-- FreqDict


type alias FreqDict =
    Dict ( Float, Float ) Int


freqDictInsertPoint : Point -> FreqDict -> FreqDict
freqDictInsertPoint point =
    Dict.update (pointToTuple point) (Maybe.map inc >> Maybe.withDefault 1 >> Just)


freqDictSingleton : Point -> FreqDict
freqDictSingleton point =
    freqDictInsertPoint point Dict.empty



-- RandomWalker


type RandomWalker
    = RandomWalker Point FreqDict


initRandomWalker : Point -> RandomWalker
initRandomWalker point =
    RandomWalker point (freqDictSingleton point)


walkSteps : Size -> Int -> RandomWalker -> Generator RandomWalker
walkSteps size steps rw =
    List.range 0 (steps - 1)
        |> List.foldl (always (Random.andThen (walk size))) (Random.constant rw)


walk : Size -> RandomWalker -> Generator RandomWalker
walk size (RandomWalker last fd) =
    let
        func nextPoint =
            RandomWalker nextPoint (freqDictInsertPoint nextPoint fd)
    in
    nextPointGenerator size last
        |> Random.map func


nextPointGenerator : Size -> Point -> Random.Generator Point
nextPointGenerator size point =
    Random.map2
        (\func by ->
            func by point
                |> constrainPointInSize size
        )
        (Random.uniform mapX [ mapY ])
        (Random.uniform inc [ dec ])



-- Model


type alias Model =
    { size : Size
    , walker : RandomWalker
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
    ( { size = size
      , walker = initRandomWalker start
      , seed = Random.initialSeed 0
      }
    , Cmd.none
    )


updateRandomWalker : Model -> Model
updateRandomWalker model =
    let
        ( nextRW, nextSeed ) =
            Random.step (walkSteps model.size 1 model.walker) model.seed
    in
    { model
        | seed = nextSeed
        , walker = nextRW
    }



-- Update


type Msg
    = NoOp
    | Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            model
                |> addCmd Cmd.none

        Tick ->
            model
                |> updateRandomWalker
                |> addCmd Cmd.none


addCmd cmd model =
    ( model, cmd )


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
    Document "Main"
        [ div [] [ text "Hello Main" ]
        , div []
            [ viewRandomWalker model.size model.walker
            ]
        ]


viewRandomWalker size (RandomWalker _ fd) =
    viewFreqDict size fd


viewFreqDict size freqDict =
    Svg.svg
        [ TypedSvg.Attributes.viewBox 0 0 size.width size.height
        , Svg.Attributes.width "50%"
        ]
        [ Svg.g [] []
        , renderFrequencyDict freqDict
        ]


renderFrequencyDict : FreqDict -> Svg.Svg msg
renderFrequencyDict =
    Dict.toList
        >> List.map
            (\( ( x, y ), freq ) ->
                ( Debug.toString ( x, y )
                , renderDot x y (freq * 5 |> atMost 100)
                )
            )
        >> Svg.Keyed.node "g" []



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
        , Svg.Attributes.opacity (String.fromInt o ++ "%")
        ]
        []



-- Basics


roundFloat : Float -> Float
roundFloat =
    round >> toFloat


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
