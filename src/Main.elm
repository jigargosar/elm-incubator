module Main exposing (main)

-- Browser.Element Scaffold

import Basics.Extra exposing (flip, uncurry)
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



-- Grid


type Grid
    = G Gwh (Dict ( Int, Int ) Cell) (List ( Int, Int ))


type Cell
    = Water


type GCE
    = GCE Int Int (Maybe Cell)


type Gwh
    = Gwh Int Int


fillG : Cell -> Int -> Int -> Grid
fillG c w h =
    let
        gd =
            rangeWh w h
                |> List.foldl (flip Dict.insert c) Dict.empty
    in
    G (Gwh w h) gd [ ( 4, 4 ) ]


toGCEList : Grid -> List GCE
toGCEList (G (Gwh w h) gd _) =
    let
        toGCE x y =
            GCE x y (Dict.get ( x, y ) gd)
    in
    rangeWh w h |> List.map (uncurry toGCE)


rangeWh : Int -> Int -> List ( Int, Int )
rangeWh w h =
    let
        fn : a -> List ( a, Int )
        fn x =
            List.range 0 (h - 1) |> List.map (Tuple.pair x)
    in
    List.range 0 (w - 1) |> List.concatMap fn


getGcs : Cwh -> Gwh -> Float
getGcs (Cwh cw ch) (Gwh gw gh) =
    min (cw * (1 / toFloat (gw + 1))) (ch * (1 / toFloat (gh + 1)))
        * 0.8


placeGridShape : Float -> Gwh -> Shape -> Shape
placeGridShape gcs (Gwh gw gh) =
    move (((toFloat gw * gcs) - gcs) * -0.5)
        (((toFloat gh * gcs) - gcs) * -0.5)


renderGrid : Cwh -> Mxy -> Grid -> List Shape
renderGrid cwh (Mxy mx my) g =
    let
        (G gwh _ _) =
            g

        gcs =
            getGcs cwh gwh
    in
    [ renderGridBg gcs gwh
    , toGCEList g
        |> List.map (renderGCE gcs)
        |> group
        |> placeGridShape gcs gwh
    , renderPointer (gcs * 0.25) mx my
    ]


renderGridBg : Float -> Gwh -> Shape
renderGridBg gcs (Gwh gw gh) =
    rectangle "lightyellow" (toFloat (gw + 1) * gcs) (toFloat (gh + 1) * gcs)


renderGCE : Float -> GCE -> Shape
renderGCE gcs (GCE x y mbc) =
    let
        rc cell =
            case cell of
                Water ->
                    group
                        [ let
                            r =
                                gcs * 0.2
                          in
                          ellipse "dodgerblue" r r
                        ]
                        |> move (toFloat x * gcs) (toFloat y * gcs)
    in
    mbc |> Maybe.map rc |> Maybe.withDefault (group [])


renderPointer : Float -> Float -> Float -> Shape
renderPointer w x y =
    group
        [ ellipse "black" 1 w
        , ellipse "black" w 1
        ]
        |> move x y



-- Model


type Model
    = M Cwh Mxy Grid


type Cwh
    = Cwh Float Float


type Mxy
    = Mxy Float Float


type alias Flags =
    { now : Int, bs : ( Float, Float ) }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        grid =
            fillG Water 10 8
    in
    ( M (flags.bs |> uncurry Cwh) (Mxy 0 0) grid
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
view (M ((Cwh cw ch) as cwh) mxy g) =
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
            (List.map draw
                (rectangle "rgba(153, 248, 255)" cw ch
                    :: rectangle "rgba(183, 169, 255)" cw ch
                    :: renderGrid cwh mxy g
                )
            )
        ]


pageMouseMoveDecoder : Decoder Msg
pageMouseMoveDecoder =
    JD.map2 OnCMM
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)


rectangle : String -> Float -> Float -> Shape
rectangle c w h =
    Rectangle w h |> Shape c [] initialTransform


ellipse : String -> Float -> Float -> Shape
ellipse c w h =
    Ellipse w h |> Shape c [] initialTransform


group : List Shape -> Shape
group ss =
    Group ss |> Shape "none" [] initialTransform


move : Float -> Float -> Shape -> Shape
move dx dy =
    mapTransform <| translateBy dx dy


mapTransform : (TF -> TF) -> Shape -> Shape
mapTransform fn (Shape c cs tx f) =
    Shape c cs (fn tx) f


type Form
    = Rectangle Float Float
    | Ellipse Float Float
    | Group (List Shape)


type Shape
    = Shape String (List String) TF Form


type TF
    = TF Float Float


initialTransform : TF
initialTransform =
    TF 0 0


translateBy : Float -> Float -> TF -> TF
translateBy dx dy (TF x y) =
    TF (x + dx) (y + dy)


draw : Shape -> HM
draw (Shape c cs (TF dx dy) s) =
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
