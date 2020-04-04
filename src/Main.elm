module Main exposing (main)

-- Browser.Element Scaffold

import Basics.Extra exposing (uncurry)
import Browser
import Browser.Events
import Dict exposing (Dict)
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
    = M Cwh Mxy Gd


type Gd
    = Gd Float Float (Dict ( Int, Int ) Cell)


type Cell
    = Water


type Cwh
    = Cwh Float Float


type Mxy
    = Mxy Float Float


type alias Flags =
    { now : Int, bs : ( Float, Float ) }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( M (flags.bs |> uncurry Cwh) (Mxy 0 0) (Gd 10 8 Dict.empty)
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
update message ((M ((Cwh cw ch) as cwh) mxy g) as model) =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotBS w h ->
            ( M (Cwh (toFloat w) (toFloat h)) mxy g, Cmd.none )

        OnCMM x y ->
            ( M cwh (Mxy (x - cw * 0.5) (y - ch * 0.5)) g, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Browser.Events.onResize GotBS ]



-- View


type alias HM =
    Html Msg


view : Model -> Html Msg
view (M (Cwh cw ch) mxy g) =
    div
        [ class "fixed absolute--fill"
        , SE.on "mousemove" pageMouseMoveDecoder
        ]
        [ Svg.svg
            [ TA.viewBox (cw * -0.5) (ch * -0.5) cw ch
            , SA.width "100%"
            , SA.height "100%"

            --, TA.preserveAspectRatio (TT.Align TT.ScaleMid TT.ScaleMid) TT.Meet
            , style "background-color" "rgba(183, 169, 255)"
            ]
            (List.map draw (drawBoard cw ch mxy g))
        ]


drawBoard : Float -> Float -> Mxy -> Gd -> List Shape
drawBoard cw ch (Mxy mx my) g =
    let
        gw =
            10

        gh =
            8

        gcw =
            min (cw * (1 / toFloat (gw + 1))) (ch * (1 / toFloat (gh + 1)))
                * 0.8

        drawCell : Int -> Int -> Shape
        drawCell x y =
            group
                [ let
                    r =
                        gcw * 0.2
                  in
                  ellipse "dodgerblue" r r
                    |> move (toFloat x * gcw) (toFloat y * gcw)
                ]

        gridCellsView =
            List.range 0 (gw - 1)
                |> List.concatMap (\x -> List.range 0 (gh - 1) |> List.map (drawCell x))
    in
    [ rectangle "rgba(153, 248, 255)" cw ch
    , rectangle "rgba(183, 169, 255)" cw ch
    , rectangle "lightyellow" (toFloat (gw + 1) * gcw) (toFloat (gh + 1) * gcw)
    , group gridCellsView
        |> move
            (((toFloat gw * gcw) - gcw) * -0.5)
            (((toFloat gh * gcw) - gcw) * -0.5)
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


rectangle : String -> Float -> Float -> Shape
rectangle c w h =
    Rectangle w h |> S c [] initialTransform


ellipse : String -> Float -> Float -> Shape
ellipse c w h =
    Ellipse w h |> S c [] initialTransform


group : List Shape -> Shape
group ss =
    Group ss |> S "none" [] initialTransform


move : Float -> Float -> Shape -> Shape
move dx dy =
    mapTransform <| translateBy dx dy


mapTransform : (TF -> TF) -> Shape -> Shape
mapTransform fn (S c cs tx f) =
    S c cs (fn tx) f


type Form
    = Rectangle Float Float
    | Ellipse Float Float
    | Group (List Shape)


type Shape
    = S String (List String) TF Form


type TF
    = TF Float Float


initialTransform : TF
initialTransform =
    TF 0 0


translateBy : Float -> Float -> TF -> TF
translateBy dx dy (TF x y) =
    TF (x + dx) (y + dy)


draw : Shape -> HM
draw (S c cs (TF dx dy) s) =
    case s of
        Rectangle w h ->
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

        Ellipse w h ->
            Svg.ellipse
                [ Px.rx w
                , Px.ry h
                , TA.transform [ TT.Translate dx dy ]
                , SA.fill c
                , TA.class cs
                ]
                []

        Group ss ->
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
