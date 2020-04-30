module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Json.Decode as D exposing (Decoder)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes exposing (fill, stroke, strokeWidth)
import TypedSvg.Attributes exposing (transform)
import TypedSvg.Attributes.InPx
import TypedSvg.Types exposing (Transform(..))



-- Input


type alias Input =
    { leftDown : Bool, rightDown : Bool, keys : Set String }


recordKey : String -> Bool -> Input -> Input
recordKey key isDown input =
    let
        keys =
            if isDown then
                Set.insert key input.keys

            else
                Set.remove key input.keys
    in
    { leftDown = Set.member "ArrowLeft" keys
    , rightDown = Set.member "ArrowRight" keys
    , keys = keys
    }


initialInput : Input
initialInput =
    Input False False Set.empty



-- Vec


type alias Vec =
    { x : Float, y : Float }


newVec : Float -> Float -> Vec
newVec x y =
    Vec x y



-- SIZE


type alias Size =
    { width : Float, height : Float }


newSize : Float -> Float -> Size
newSize w h =
    Size w h


viewBoxOfSize : Size -> Svg.Attribute msg
viewBoxOfSize size =
    TypedSvg.Attributes.viewBox
        (-size.width / 2)
        (-size.height / 2)
        size.width
        size.height



-- Paddle


type alias Paddle =
    { pos : Vec
    , size : Size
    }


initPaddle : Size -> Paddle
initPaddle canvasSize =
    let
        size =
            newSize 80 10

        pos =
            newVec 0 ((canvasSize.height * 0.5) - 20)
    in
    Paddle pos size


viewPaddle : Paddle -> Svg msg
viewPaddle paddle =
    rect
        paddle.size.width
        paddle.size.height
        [ transform [ Translate paddle.pos.x paddle.pos.y ]
        ]



-- Model


type alias Model =
    { input : Input
    , paddle : Paddle
    , canvasSize : Size
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    let
        canvasSize =
            newSize 600 300
    in
    ( { input = initialInput
      , paddle = initPaddle canvasSize
      , canvasSize = canvasSize
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | OnKeyDown String
    | OnKeyUp String
    | Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        OnKeyDown key ->
            ( { model | input = recordKey key True model.input }, Cmd.none )

        OnKeyUp key ->
            ( { model | input = recordKey key False model.input }, Cmd.none )

        Tick ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown (D.field "key" D.string |> D.map OnKeyDown)
        , Browser.Events.onKeyUp (D.field "key" D.string |> D.map OnKeyUp)
        , Browser.Events.onAnimationFrameDelta (always Tick)
        ]



-- View


type alias DM =
    Document Msg


view : Model -> DM
view model =
    let
        canvasSize =
            model.canvasSize
    in
    Document "Brick Breaker"
        [ Svg.svg
            [ viewBoxOfSize canvasSize
            , fill "none"
            , stroke "gray"
            , strokeWidth "1"
            ]
            [ rect canvasSize.width canvasSize.height []
            , viewPaddle model.paddle
            ]
        ]



-- DRAW


rect w h attrs =
    Svg.rect
        ([ TypedSvg.Attributes.InPx.x (-w / 2)
         , TypedSvg.Attributes.InPx.y (-h / 2)
         , TypedSvg.Attributes.InPx.width w
         , TypedSvg.Attributes.InPx.height h
         ]
            ++ attrs
        )
        []



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
