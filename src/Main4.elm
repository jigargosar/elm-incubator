module Main4 exposing (main)

import Anim as Anim
import Basics.Extra exposing (uncurry)
import Browser
import Browser.Events
import Dict exposing (Dict)
import Dict.Extra
import Html exposing (Html)
import List.Extra exposing (find)
import Process
import Svg exposing (rect, svg)
import Svg.Attributes as SA
import Task
import TypedSvg.Attributes exposing (transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, r, width)
import TypedSvg.Types exposing (Opacity(..), Transform(..))


type alias Anim =
    Anim.Anim


gridColumns =
    7


gridRows =
    5


minGridIdx =
    0


maxGridIdx =
    (gridRows * gridColumns) - 1


validateIdx idx =
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



-- Model


type alias Idx =
    Int


type Model
    = Idle
    | Connecting ConnectingState
    | Dragging (List Idx) DraggingViewState


type alias DraggingViewState =
    List ( Idx, Anim )


type alias ConnectingState =
    Dict Idx ConnectingCellState


type ConnectingCellState
    = CellConnecting Anim
    | CellDisconnecting Anim
    | CellConnected


initConnectingState : Idx -> ConnectingState
initConnectingState idx =
    Dict.singleton idx initCellConnecting


initCellConnecting =
    CellConnecting (Anim.initAnim 1 0.5)


initCellDisconnecting =
    CellDisconnecting (Anim.initAnim 0.5 1)


retargetCellConnecting a =
    CellConnecting (Anim.retarget 0.5 a)


retargetCellDisconnecting a =
    CellDisconnecting (Anim.retarget 1 a)


tickConnectingState : Float -> ConnectingState -> ConnectingState
tickConnectingState delta dict =
    Dict.Extra.filterMap
        (\_ cc ->
            case cc of
                CellConnecting a ->
                    let
                        na =
                            Anim.animTick delta a
                    in
                    if Anim.isDone na then
                        Just CellConnected

                    else
                        Just (CellConnecting na)

                CellDisconnecting a ->
                    let
                        na =
                            Anim.animTick delta a
                    in
                    if Anim.isDone na then
                        Nothing

                    else
                        Just (CellDisconnecting na)

                CellConnected ->
                    Just CellConnected
        )
        dict


renderConnectingState : ConnectingState -> SM
renderConnectingState dict =
    let
        func idx =
            case Dict.get idx dict of
                Just cc ->
                    case cc of
                        CellConnected ->
                            renderIdxWith idx [ scale 0.5 ]

                        CellConnecting anim ->
                            renderIdxWith idx [ scale (Anim.animValue anim) ]

                        CellDisconnecting anim ->
                            renderIdxWith idx [ scale (Anim.animValue anim) ]

                Nothing ->
                    renderIdx idx
    in
    renderBatch (List.map func gridIndices)


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
                            offset + 400
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

                Connecting connectingState ->
                    Connecting (tickConnectingState delta connectingState)

                Dragging di vs ->
                    List.map (Tuple.mapSecond (Anim.animTick delta)) vs
                        |> Dragging di
            , Cmd.none
            )

        OnDrag unverifiedIdx ->
            ( case ( model, validateIdx unverifiedIdx ) of
                ( Idle, Just idx ) ->
                    Connecting (initConnectingState idx)

                ( Connecting d, Just idx ) ->
                    case Dict.get idx d of
                        Nothing ->
                            Dict.insert idx initCellConnecting d
                                |> Connecting

                        Just cc ->
                            Dict.insert idx
                                (case cc of
                                    CellConnecting anim ->
                                        retargetCellDisconnecting anim

                                    CellDisconnecting anim ->
                                        retargetCellConnecting anim

                                    CellConnected ->
                                        initCellDisconnecting
                                )
                                d
                                |> Connecting

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
                renderBatch (List.map renderIdx gridIndices)

            Connecting connectingState ->
                renderConnectingState connectingState

            Dragging _ draggingViewState ->
                renderDraggingViewState draggingViewState
        ]


renderDraggingViewState : DraggingViewState -> SM
renderDraggingViewState vs =
    let
        func idx =
            case find (firstEq idx) vs of
                Just ( _, a ) ->
                    renderIdxWith idx [ scale (Anim.animValue a) ]

                Nothing ->
                    renderIdx idx
    in
    renderBatch (List.map func gridIndices)


type alias SM =
    Svg.Svg Msg



--noinspection ElmUnusedSymbol


firstEq expected ( actual, _ ) =
    expected == actual


renderIdx idx =
    renderIdxWith idx []


renderIdxWith idx with =
    circle "#46a4ff" (gridCellWidth * 0.3) (moveToIdx idx :: with)


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


renderBatch : List (Svg.Svg msg) -> Svg.Svg msg
renderBatch =
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
--noinspection ElmUnusedSymbol


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



--noinspection ElmUnusedSymbol


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
