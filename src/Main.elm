module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Html.Events as E
import Json.Decode as JD exposing (Decoder)
import Svg
import Svg.Attributes as SA
import Svg.Events as SE
import Task
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types as TT



-- Model


type Model
    = M CX COM


type CX
    = CX Float Float Float Float


type
    COM
    -- canvas offset mouse x y
    = COM Float Float


type alias Flags =
    { now : Int, bs : ( Int, Int ) }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( M (CX 0 0 600 600) (COM 0 0)
    , getAll
    )


type alias CM =
    Cmd Msg


getAll : CM
getAll =
    let
        getCanvasEl : CM
        getCanvasEl =
            Dom.getElement "canvas" |> Task.attempt GotCanvasEl
    in
    Cmd.batch [ getCanvasEl ]



-- Update


type Msg
    = NoOp
    | GotCanvasEl (Result Dom.Error Dom.Element)
    | GotBS Int Int
    | OnCMM Float Float


update : Msg -> Model -> ( Model, Cmd Msg )
update message ((M cx com) as model) =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotCanvasEl (Err (Dom.NotFound domId)) ->
            let
                _ =
                    Debug.log "canvas element not found" domId
            in
            ( model, Cmd.none )

        GotCanvasEl (Ok el) ->
            let
                { x, y, width, height } =
                    el.element
            in
            ( M (CX x y width height) com, Cmd.none )

        GotBS _ _ ->
            ( model, getAll )

        OnCMM x y ->
            ( M cx (COM x y), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Browser.Events.onResize GotBS ]



-- View


type alias HM =
    Html Msg


toMXY : CX -> COM -> ( Float, Float )
toMXY (CX ox oy w h) (COM x y) =
    ( (x - ox) / w * swPx - (swPx / 2), (y - oy) / h * shPx - (shPx / 2) )


swPx =
    800


shPx =
    600


view : Model -> Html Msg
view (M cx com) =
    let
        ( mx, my ) =
            toMXY cx com

        _ =
            Debug.log "(mx,my)" ( mx, my )

        gw =
            10

        gh =
            8

        gcwPx =
            50

        drawCell x y =
            group
                [ let
                    r =
                        gcwPx * 0.2
                  in
                  ellipse "dodgerblue" r r
                    |> move (toFloat x * gcwPx) (toFloat y * gcwPx)
                ]

        gridCellsView =
            List.range 0 (gw - 1)
                |> List.concatMap (\x -> List.range 0 (gh - 1) |> List.map (drawCell x))

        svgView =
            Svg.svg
                [ TA.viewBox (swPx * -0.5) (shPx * -0.5) swPx shPx
                , TA.class [ "flex-auto" ]
                , TA.preserveAspectRatio (TT.Align TT.ScaleMid TT.ScaleMid) TT.Meet
                , style "background-color" "rgba(183, 169, 255)"
                ]
                [ Svg.g [ SA.id "canvas" ]
                    [ draw <|
                        group
                            [ rectangle "rgba(153, 248, 255)" swPx shPx
                            , rectangle "lightyellow" (toFloat (gw + 1) * gcwPx) (toFloat (gh + 1) * gcwPx)
                            , group gridCellsView
                                |> move
                                    (((toFloat gw * gcwPx) - gcwPx) * -0.5)
                                    (((toFloat gh * gcwPx) - gcwPx) * -0.5)
                            , group
                                [ ellipse "black" 1 10
                                , ellipse "black" 10 1
                                ]
                                |> move mx my
                            ]
                    ]
                ]
    in
    div
        [ class "fixed absolute--fill flex"
        , SE.on "mousemove" pageMouseMoveDecoder
        ]
        [ svgView
        ]


pageMouseMoveDecoder : Decoder Msg
pageMouseMoveDecoder =
    JD.map2 OnCMM
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)


rectangle : String -> Float -> Float -> S
rectangle c w h =
    R w h |> S c initialTransform


ellipse : String -> Float -> Float -> S
ellipse c w h =
    E w h |> S c initialTransform


group : List S -> S
group ss =
    G ss |> S "none" initialTransform


move : Float -> Float -> S -> S
move dx dy =
    mapTransform <| translateBy dx dy


mapTransform : (TF -> TF) -> S -> S
mapTransform fn (S c tx f) =
    S c (fn tx) f


type F
    = R Float Float
    | E Float Float
    | G (List S)


type S
    = S String TF F


type TF
    = TF Float Float


initialTransform : TF
initialTransform =
    TF 0 0


translateBy : Float -> Float -> TF -> TF
translateBy dx dy (TF x y) =
    TF (x + dx) (y + dy)


draw : S -> HM
draw (S c (TF dx dy) s) =
    case s of
        R w h ->
            Svg.rect
                [ Px.width w
                , Px.height h
                , TA.transform
                    [ TT.Translate (w * -0.5) (h * -0.5)
                    , TT.Translate dx dy
                    ]
                , SA.fill c
                ]
                []

        E w h ->
            Svg.ellipse
                [ Px.rx w
                , Px.ry h
                , TA.transform [ TT.Translate dx dy ]
                , SA.fill c
                ]
                []

        G ss ->
            Svg.g
                [ TA.transform [ TT.Translate dx dy ]
                , SA.fill c
                ]
                (List.map draw ss)



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
