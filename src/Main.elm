module Main exposing (main)

-- Browser.Element Scaffold

import Basics.Extra exposing (uncurry)
import Browser
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events as E
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra
import Json.Encode exposing (Value)
import Svg
import Svg.Attributes as SA
import Task
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types as TT



-- Model


type BS
    = BS Int Int


type Model
    = M BS Float Float


type alias Flags =
    { now : Int, bs : ( Int, Int ) }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( M (uncurry BS flags.bs) 600 600
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
    Cmd.batch [ getCanvasEl, Dom.getViewport |> Task.perform GotVP ]



-- Update


type Msg
    = NoOp
    | GotCanvasEl (Result Dom.Error Dom.Element)
    | GotVP Dom.Viewport
    | GotBS Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
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
                _ =
                    Debug.log "el.element" el
            in
            ( model, Cmd.none )

        GotBS w h ->
            let
                _ =
                    Debug.log "GotBS" ( w, h )
            in
            ( model, getAll )

        GotVP viewport ->
            let
                _ =
                    Debug.log "GotVP" viewport
            in
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Browser.Events.onResize GotBS ]



-- View


type alias HM =
    Html Msg


view : Model -> Html Msg
view _ =
    let
        swPx =
            600

        shPx =
            600

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
                , TA.class [ "flex-auto pa5" ]
                , E.on "mousemove" logOffset
                , TA.id "canvas"
                ]
                [ Svg.rect [ SA.width "100%", SA.height "100%", SA.fill "lightblue" ] []
                , draw <|
                    group
                        [ --group
                          --    [ rectangle "dodgerblue" 200 200
                          --        |> move -100 -100
                          --    , ellipse "red" 100 100
                          --        |> move -100 -100
                          --    ]
                          --,
                          rectangle "lightblue" swPx shPx
                        , rectangle "lightyellow" (toFloat (gw + 1) * gcwPx) (toFloat (gh + 1) * gcwPx)
                        , group gridCellsView
                            |> move
                                (((toFloat gw * gcwPx) - gcwPx) * -0.5)
                                (((toFloat gh * gcwPx) - gcwPx) * -0.5)
                        ]
                ]
    in
    div [ class "flex" ]
        [ svgView
        ]


logOffset : Decoder Msg
logOffset =
    JD.map2 Tuple.pair
        (JD.field "offsetX" JD.int)
        (JD.field "offsetY" JD.int)
        |> JD.andThen
            (\v ->
                let
                    _ =
                        Debug.log "v" v
                in
                JD.fail ""
            )


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
