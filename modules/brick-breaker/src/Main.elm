module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Json.Decode as D exposing (Decoder)
import Set exposing (Set)
import Svg
import Svg.Attributes exposing (fill, stroke, strokeWidth)
import TypedSvg.Attributes exposing (transform)
import TypedSvg.Attributes.InPx
import TypedSvg.Types



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



-- Model


type alias Model =
    { input : Input }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { input = initialInput }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | OnKeyDown String
    | OnKeyUp String


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        OnKeyDown key ->
            ( { model | input = recordKey key True model.input }, Cmd.none )

        OnKeyUp key ->
            ( { model | input = recordKey key False model.input }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown (D.field "key" D.string |> D.map OnKeyDown)
        , Browser.Events.onKeyUp (D.field "key" D.string |> D.map OnKeyUp)
        ]



-- View


type alias DM =
    Document Msg


view : Model -> DM
view _ =
    let
        size =
            newSize 600 300

        canvasBounds =
            newBoundsFromSize size
    in
    Document "Brick Breaker"
        [ Svg.svg
            [ viewBoxOfSize size
            , fill "none"
            , stroke "gray"
            , strokeWidth "1"
            ]
            [ rect size.width size.height []
            , rect
                80
                10
                [ transform
                    [ moveY canvasBounds.bottom
                    , moveUp 15
                    ]
                ]
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



-- Bounds


type alias Bounds =
    { left : Float
    , right : Float
    , top : Float
    , bottom : Float
    }


newBoundsFromSize : Size -> Bounds
newBoundsFromSize size =
    Bounds (-size.width / 2) (size.width / 2) (-size.height / 2) (size.height / 2)



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
