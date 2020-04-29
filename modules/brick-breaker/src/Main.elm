module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, text)
import Svg
import Svg.Attributes
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx
import TypedSvg.Types



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
    let
        size =
            newSize 600 400
    in
    Document "Brick Breaker"
        [ Svg.svg
            [ viewBoxOfSize size
            , Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "gray"
            , Svg.Attributes.strokeWidth "1"
            ]
            [ rect size.width size.height []
            , rect 80 20 [ TypedSvg.Attributes.transform [ moveDown (size.height / 2 - 20) ] ]
            ]
        ]


move =
    TypedSvg.Types.Translate


moveY dy =
    move 0 dy


moveDown =
    moveY


moveUp =
    negate >> moveDown



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



-- SIZE


type alias Size =
    { width : Float, height : Float }


newSize : Float -> Float -> Size
newSize w h =
    Size w h


viewBoxOfSize : Size -> Svg.Attribute msg
viewBoxOfSize size =
    TypedSvg.Attributes.viewBox (-size.width / 2) (-size.height / 2) size.width size.height



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
