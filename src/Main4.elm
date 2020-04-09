module Main4 exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Svg exposing (rect, svg)
import Svg.Attributes as SA
import TypedSvg.Attributes exposing (transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, r, width)
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
    svg
        [ viewBox screenLeft screenTop screenWidth screenHeight
        , width screenWidth
        , height screenHeight
        ]
        [ rect "#ffc973" screenWidth screenHeight []
        , circle "#46a4ff" 100 []
        ]



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SVG PUBLIC API


square : String -> Float -> List (Shape -> Shape) -> Svg.Svg msg
square c w =
    rect c w w


rect : String -> Float -> Float -> List (Shape -> Shape) -> Svg.Svg msg
rect color w h fnList =
    let
        m =
            Rectangle (initRectRecord color w h)
    in
    List.foldl (<|) m fnList
        |> renderShape


circle : String -> Float -> List (Shape -> Shape) -> Svg.Svg msg
circle color r fnList =
    let
        m =
            Circle (initCircleRecord color r)
    in
    List.foldl (<|) m fnList |> renderShape


fade : Float -> Shape -> Shape
fade o shape =
    case shape of
        Rectangle m ->
            Rectangle { m | o = o }

        Circle m ->
            Circle { m | o = o }



-- SVG PRIVATE API


type Shape
    = Rectangle RectangleRecord
    | Circle CircleRecord


type alias RectangleRecord =
    { x : Float
    , y : Float
    , s : Float
    , o : Float
    , fill : String
    , w : Float
    , h : Float
    }


type alias CircleRecord =
    { x : Float
    , y : Float
    , s : Float
    , o : Float
    , fill : String
    , r : Float
    }


initRectRecord : String -> Float -> Float -> RectangleRecord
initRectRecord =
    RectangleRecord 0 0 1 1


initCircleRecord : String -> Float -> CircleRecord
initCircleRecord =
    CircleRecord 0 0 1 1


renderShape : Shape -> Svg.Svg msg
renderShape shape =
    case shape of
        Rectangle m ->
            renderRectRecord m

        Circle m ->
            renderCircleRecord m


renderRectRecord : RectangleRecord -> Svg.Svg msg
renderRectRecord m =
    Svg.rect
        [ width m.w
        , height m.h
        , SA.fill m.fill
        , transform <| renderRectTransform m
        , opacity m.o
        ]
        []


renderCircleRecord : CircleRecord -> Svg.Svg msg
renderCircleRecord m =
    Svg.circle
        [ r m.r
        , SA.fill m.fill
        , transform <| renderTransform m
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
