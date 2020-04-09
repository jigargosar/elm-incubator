module Main4 exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Svg exposing (rect, svg)
import Svg.Attributes exposing (fill, opacity)
import TypedSvg.Attributes exposing (transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width)
import TypedSvg.Types exposing (Opacity(..), Transform(..))


gridColumns =
    7


gridRows =
    5


minGridIdx =
    0


maxGridIdx =
    (gridRows * gridColumns) - 1


validIdx idx =
    if clamp minGridIdx maxGridIdx idx == idx then
        Just idx

    else
        Nothing


gridCellWidth =
    50


gridIndices =
    List.range minGridIdx maxGridIdx


gridCellIndices : List Int
gridCellIndices =
    List.range 1 (gridRows * gridColumns)



-- Model


type alias Model =
    {}


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( {}
    , Cmd.none
    )



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
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Browser.Events.onAnimationFrame (\_ -> Tick) ]



-- View


screenWidth =
    600


screenHeight =
    800


screenLeft =
    screenWidth * -0.5


screenTop =
    screenHeight * -0.5


view : Model -> Html Msg
view _ =
    svg [ viewBox screenLeft screenTop screenWidth screenHeight, width screenWidth, height screenHeight ]
        [ rect [ width screenWidth, height screenHeight, fill "orange", opacity 0.5 ] [] ]


type Rectangle
    = Rectangle RectangleRecord


type alias RectangleRecord =
    { x : Float
    , y : Float
    , s : Float
    , o : Float
    , fill : String
    , w : Float
    , h : Float
    }


renderRect : Rectangle -> Svg.Svg msg
renderRect (Rectangle m) =
    rect
        [ width m.w
        , height m.h
        , fill m.fill
        , transform <| renderRectTransform m
        , opacity m.o
        ]
        []


renderRectTransform m =
    Translate (m.w * -0.5) (m.h * -0.5)
        :: renderTransform m


renderTransform m =
    [ Translate m.x m.y, Scale m.s m.s ]


opacity =
    TypedSvg.Attributes.opacity << Opacity



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
