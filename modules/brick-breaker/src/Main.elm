module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Json.Decode as D exposing (Decoder)
import Svg
import Svg.Attributes
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx
import TypedSvg.Types



-- Model


type alias Model =
    { input : Input }


type alias Input =
    { leftDown : Bool, rightDown : Bool }


initialInput =
    Input False False


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
    | OnKeyDown Key
    | OnKeyUp Key


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        OnKeyDown key ->
            ( { model | input = recordKeyDown key model.input }, Cmd.none )

        OnKeyUp key ->
            ( { model | input = recordKeyUp key model.input }, Cmd.none )


recordKeyDown : Key -> Input -> Input
recordKeyDown key input =
    case key of
        ArrowLeft ->
            { input | leftDown = True }

        ArrowRight ->
            { input | rightDown = True }


recordKeyUp : Key -> Input -> Input
recordKeyUp key input =
    case key of
        ArrowLeft ->
            { input | leftDown = False }

        ArrowRight ->
            { input | rightDown = False }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown
            (keyEventDecoder
                |> D.andThen
                    (\ev ->
                        case ev.key of
                            "ArrowLeft" ->
                                D.succeed ArrowLeft

                            "ArrowRight" ->
                                D.succeed ArrowRight

                            _ ->
                                D.fail ""
                    )
                |> D.map OnKeyDown
            )
        ]


type alias KeyEvent =
    { key : String
    }


keyEventDecoder : Decoder KeyEvent
keyEventDecoder =
    D.succeed KeyEvent
        |> D.map2 (|>) (D.field "key" D.string)


type Key
    = ArrowLeft
    | ArrowRight



-- View


type alias DM =
    Document Msg


view : Model -> DM
view _ =
    let
        size =
            newSize 600 400

        canvasBounds =
            newBoundsFromSize size
    in
    Document "Brick Breaker"
        [ Svg.svg
            [ viewBoxOfSize size
            , Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "gray"
            , Svg.Attributes.strokeWidth "1"
            ]
            [ rect size.width size.height []
            , rect
                80
                20
                [ TypedSvg.Attributes.transform
                    [ moveY canvasBounds.bottom
                    , moveUp 20
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
