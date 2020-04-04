module Main exposing (main)

-- Browser.Element Scaffold

import Basics.Extra exposing (uncurry)
import Browser
import Browser.Events
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Json.Decode as JD exposing (Decoder)
import Svg
import Svg.Attributes as SA
import Svg.Events as SE
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types as TT



-- Model


type Model
    = M CX MXY


type CX
    = CX Float Float Float Float


type MXY
    = MXY Float Float


type alias Flags =
    { now : Int, bs : ( Float, Float ) }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( M (flags.bs |> uncurry (CX 0 0)) (MXY 0 0)
    , Cmd.none
    )


type alias CM =
    Cmd Msg



-- Update


type Msg
    = NoOp
    | GotBS Int Int
    | OnCMM Float Float


update : Msg -> Model -> ( Model, Cmd Msg )
update message ((M cx com) as model) =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotBS w h ->
            ( M (CX 0 0 (toFloat w) (toFloat h)) com, Cmd.none )

        OnCMM x y ->
            ( M cx (MXY x y), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Browser.Events.onResize GotBS ]



-- View


type alias HM =
    Html Msg


view : Model -> Html Msg
view (M ((CX _ _ swPx shPx) as cx) com) =
    let
        ( mx, my ) =
            toMXY cx com

        toMXY : CX -> MXY -> ( Float, Float )
        toMXY _ (MXY x y) =
            ( x - swPx * 0.5, y - shPx * 0.5 )
    in
    div
        [ class "fixed absolute--fill"
        , SE.on "mousemove" pageMouseMoveDecoder
        ]
        [ Svg.svg
            [ TA.viewBox (swPx * -0.5) (shPx * -0.5) swPx shPx
            , SA.width "100%"
            , SA.height "100%"

            --, TA.preserveAspectRatio (TT.Align TT.ScaleMid TT.ScaleMid) TT.Meet
            , style "background-color" "rgba(183, 169, 255)"
            ]
            (List.map draw (drawBoard swPx shPx mx my))
        ]


drawBoard : Float -> Float -> Float -> Float -> List S
drawBoard swPx shPx mx my =
    let
        gw =
            10

        gh =
            8

        gcwPx =
            min (swPx * (1 / toFloat (gw + 1))) (shPx * (1 / toFloat (gh + 1)))
                * 0.8

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
    in
    [ rectangle "rgba(153, 248, 255)" swPx shPx
    , rectangle "rgba(183, 169, 255)" swPx shPx
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


pageMouseMoveDecoder : Decoder Msg
pageMouseMoveDecoder =
    JD.map2 OnCMM
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)


rectangle : String -> Float -> Float -> S
rectangle c w h =
    R w h |> S c [] initialTransform


ellipse : String -> Float -> Float -> S
ellipse c w h =
    E w h |> S c [] initialTransform


group : List S -> S
group ss =
    G ss |> S "none" [] initialTransform


move : Float -> Float -> S -> S
move dx dy =
    mapTransform <| translateBy dx dy


mapTransform : (TF -> TF) -> S -> S
mapTransform fn (S c cs tx f) =
    S c cs (fn tx) f


type F
    = R Float Float
    | E Float Float
    | G (List S)


type S
    = S String (List String) TF F


type TF
    = TF Float Float


initialTransform : TF
initialTransform =
    TF 0 0


translateBy : Float -> Float -> TF -> TF
translateBy dx dy (TF x y) =
    TF (x + dx) (y + dy)


draw : S -> HM
draw (S c cs (TF dx dy) s) =
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
                , TA.class cs
                ]
                []

        E w h ->
            Svg.ellipse
                [ Px.rx w
                , Px.ry h
                , TA.transform [ TT.Translate dx dy ]
                , SA.fill c
                , TA.class cs
                ]
                []

        G ss ->
            Svg.g
                [ TA.transform [ TT.Translate dx dy ]
                , SA.fill c
                , TA.class cs
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
