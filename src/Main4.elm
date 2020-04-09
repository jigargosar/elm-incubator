module Main4 exposing (main)

import Basics.Extra exposing (uncurry)
import Browser
import Browser.Events
import Html exposing (Html)
import List.Extra exposing (find)
import Process
import Svg exposing (rect, svg)
import Svg.Attributes as SA
import Task
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


gridWidth =
    toFloat gridColumns * gridCellWidth


gridHeight =
    toFloat gridRows * gridCellWidth


gridIndices : List Int
gridIndices =
    List.range minGridIdx maxGridIdx



-- ANIM


type alias Anim =
    { from : Float, to : Float, duration : Float, elapsed : Float }


initAnim : Float -> Float -> Anim
initAnim from to =
    { from = from, to = to, duration = 200, elapsed = 0 }


animReverse : Anim -> Anim
animReverse ({ from, to, duration, elapsed } as anim) =
    { anim | from = to, to = from, elapsed = duration - elapsed }


animProgress : Anim -> Float
animProgress { from, to, duration, elapsed } =
    if from == to || duration <= 0 || elapsed >= duration then
        1

    else
        elapsed / duration


animIsDone : Anim -> Bool
animIsDone anim =
    animProgress anim == 1


animValue : Anim -> Float
animValue ({ from, to, duration, elapsed } as anim) =
    (to - from) * animProgress anim + from


animTick : Float -> Anim -> Anim
animTick delta ({ elapsed } as anim) =
    if animIsDone anim then
        anim

    else
        { anim | elapsed = elapsed + delta }



-- Model


type alias Idx =
    Int


type Model
    = Idle
    | Dragging (List ( Idx, Anim ))


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        dragIndices =
            [ 9, 10, 11, 11, 10, 9, 9, 10, 11, 12, 19, 26, 25, 24 ]

        dragCmds =
            List.foldl
                (\idx ( offset, ms ) ->
                    let
                        newOffset =
                            offset + 200
                    in
                    ( newOffset, delay newOffset (OnDrag idx) :: ms )
                )
                ( 0, [] )
                dragIndices
                |> Tuple.second
    in
    ( Idle
    , Cmd.batch dragCmds
    )


delay ms msg =
    Process.sleep ms |> Task.perform (\_ -> msg)



-- Update


type Msg
    = NoOp
    | DeltaTick Float
    | OnDrag Idx


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        DeltaTick delta ->
            ( case model of
                Idle ->
                    model

                Dragging list ->
                    Dragging (List.map (Tuple.mapSecond (animTick delta)) list)
            , Cmd.none
            )

        OnDrag unverifiedIdx ->
            let
                initDragAnim =
                    initAnim 1 0.5
            in
            ( case ( model, validIdx unverifiedIdx ) of
                ( Idle, Just idx ) ->
                    Dragging [ ( idx, initDragAnim ) ]

                ( Dragging list, Just idx ) ->
                    case find (firstEq idx) list of
                        Just ( _, anim ) ->
                            Dragging (( idx, animReverse anim ) :: list)

                        Nothing ->
                            Dragging (( idx, initDragAnim ) :: list)

                _ ->
                    model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Browser.Events.onAnimationFrameDelta DeltaTick ]



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
view model =
    svg
        [ viewBox screenLeft screenTop screenWidth screenHeight
        , width screenWidth
        , height screenHeight
        ]
        [ rect "#ffc973" screenWidth screenHeight []
        , case model of
            Idle ->
                batch (List.map renderIdx gridIndices)

            Dragging list ->
                let
                    func idx =
                        case find (firstEq idx) list of
                            Just ( _, anim ) ->
                                renderIdxAnim idx anim

                            Nothing ->
                                renderIdx idx
                in
                batch (List.map func gridIndices)
        ]


firstEq expected ( actual, _ ) =
    expected == actual


renderIdx idx =
    circle "#46a4ff" (gridCellWidth * 0.3) [ moveToIdx idx ]


renderIdxAnim idx anim =
    circle "#46a4ff" (gridCellWidth * 0.3) [ moveToIdx idx, scale (animValue anim) ]


moveToIdx idx =
    uncurry move (idxToXY idx)


idxToXY : Int -> ( Float, Float )
idxToXY idx =
    let
        xi =
            modBy gridColumns idx

        yi =
            idx // gridColumns

        dx =
            (gridWidth - gridCellWidth) * -0.5

        dy =
            (gridHeight - gridCellWidth) * -0.5

        x =
            toFloat xi * gridCellWidth + dx

        y =
            toFloat yi * gridCellWidth + dy
    in
    ( x, y )


batch =
    Svg.g []



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


move : Float -> Float -> Shape -> Shape
move dx dy shape =
    case shape of
        Rectangle m ->
            Rectangle (moveRecord dx dy m)

        Circle m ->
            Circle (moveRecord dx dy m)


scale : Float -> Shape -> Shape
scale s shape =
    case shape of
        Rectangle m ->
            Rectangle (scaleRecord s m)

        Circle m ->
            Circle (scaleRecord s m)



-- SVG PRIVATE API


moveRecord dx dy ({ x, y } as m) =
    { m | x = x + dx, y = y + dy }


scaleRecord ns ({ s } as m) =
    { m | s = s * ns }


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
