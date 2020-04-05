module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Json.Decode as JD exposing (Decoder)
import List.Extra exposing (scanl)
import Svg
import Svg.Attributes as SA
import Svg.Events as SE
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types as TT



-- FF Float Float


type F2
    = F2 Float Float


ffFromTuple : ( Float, Float ) -> F2
ffFromTuple ( a, b ) =
    F2 a b


iiToFloat : I2 -> F2
iiToFloat (I2 a b) =
    F2 (toFloat a) (toFloat b)



-- II Int Int


type I2
    = I2 Int Int


iiApply2 : (Int -> Int -> a) -> I2 -> a
iiApply2 func (I2 a b) =
    func a b


iiRight : I2 -> I2
iiRight (I2 x y) =
    I2 (x + 1) y


iiDown : I2 -> I2
iiDown (I2 x y) =
    I2 x (y + 1)


iiRange : I2 -> List I2
iiRange (I2 w h) =
    let
        fn : Int -> List I2
        fn x =
            List.range 0 (h - 1) |> List.map (I2 x)
    in
    List.range 0 (w - 1) |> List.concatMap fn


iiToPair : I2 -> ( Int, Int )
iiToPair =
    iiApply2 Tuple.pair



-- I2Dict


type IIDict a
    = IIDict (Dict ( Int, Int ) a)



--iidEmpty : IIDict a
--iidEmpty =
--    IIDict Dict.empty


iidFromList : List ( I2, a ) -> IIDict a
iidFromList =
    List.map (Tuple.mapFirst iiToPair) >> Dict.fromList >> IIDict



--iidInsert : II -> a -> IIDict a -> IIDict a
--iidInsert ii a (IIDict d) =
--    IIDict (Dict.insert (iiToPair ii) a d)


iidGet : I2 -> IIDict a -> Maybe a
iidGet ii (IIDict d) =
    Dict.get (iiToPair ii) d



-- Grid


type Grid
    = G Gwh (IIDict Cell) (List I2)


type Cell
    = Water


type GCE
    = GCE I2 RCell


type RCell
    = REmpty
    | RWater Bool


type Gwh
    = Gwh I2


fillG : Cell -> Int -> Int -> Grid
fillG c w h =
    let
        gd =
            iiRange (I2 w h)
                |> List.map (\xy -> ( xy, c ))
                |> iidFromList

        --l1 = [ ( 2, 2 ), ( 3, 2 ), ( 4, 2 ), ( 4, 3 ), ( 4, 4 ) ]
        l2 =
            scanl (<|) (I2 2 2) [ iiRight, iiRight, iiDown, iiDown ]
    in
    G (Gwh (I2 w h)) gd l2


toGCEList : Grid -> List GCE
toGCEList (G (Gwh wh) gd ds) =
    let
        toGCE xy =
            GCE xy
                (case iidGet xy gd of
                    Nothing ->
                        REmpty

                    Just Water ->
                        RWater (List.member xy ds)
                )
    in
    iiRange wh |> List.map toGCE


getGcs : Cwh -> Gwh -> Float
getGcs (Cwh (F2 cw ch)) (Gwh (I2 gw gh)) =
    min (cw * (1 / toFloat (gw + 1))) (ch * (1 / toFloat (gh + 1)))
        * 0.8


giToC : Float -> I2 -> Gwh -> F2
giToC gcs xy (Gwh wh) =
    let
        (F2 x y) =
            iiToFloat xy

        (F2 w h) =
            iiToFloat wh
    in
    F2 (((w * gcs) - gcs) * -0.5 + x * gcs) (((h * gcs) - gcs) * -0.5 + y * gcs)


placeGridShape : Float -> Gwh -> Shape -> Shape
placeGridShape gcs (Gwh (I2 gw gh)) =
    move (((toFloat gw * gcs) - gcs) * -0.5)
        (((toFloat gh * gcs) - gcs) * -0.5)


renderGrid : Cwh -> Mxy -> Grid -> List Shape
renderGrid cwh ((Mxy mx my) as mxy) g =
    let
        (G gwh _ _) =
            g

        gcs =
            getGcs cwh gwh

        gceList =
            toGCEList g

        renderGridCellEntries : List GCE -> Shape
        renderGridCellEntries l =
            l
                |> List.map (renderGCE gcs)
                |> group
                |> placeGridShape gcs gwh
    in
    [ renderGridBg gcs gwh
    , renderGridConnections gcs g |> placeGridShape gcs gwh
    , renderGridCellEntries gceList
    , renderConnectionToMouse mxy gcs g
    , renderGridCellEntries (filterLastShape g gceList)
    , renderPointer (gcs * 0.25) mx my
    ]


filterLastShape : Grid -> List GCE -> List GCE
filterLastShape (G _ _ conPts) =
    let
        maybeLastPt =
            List.Extra.last conPts
    in
    List.filter (\(GCE xy _) -> Just xy == maybeLastPt)


renderConnectionToMouse : Mxy -> Float -> Grid -> Shape
renderConnectionToMouse (Mxy mx my) gcs ((G gwh _ conPts) as g) =
    case List.Extra.last conPts of
        Just p1 ->
            let
                (F2 x1 y1) =
                    giToC gcs p1 gwh
            in
            group
                [ connectionPolyLine gcs [ ( x1, y1 ), ( mx, my ) ]
                ]

        Nothing ->
            group []


connectionPolyLine : Float -> List ( Float, Float ) -> Shape
connectionPolyLine gcs =
    polyLine "green" (gcs * 0.03)


renderGridConnections : Float -> Grid -> Shape
renderGridConnections gcs (G _ _ conPts) =
    let
        idxToPt (I2 a b) =
            ( toFloat a * gcs, toFloat b * gcs )

        r1 =
            connectionPolyLine gcs (List.map idxToPt conPts)
    in
    group [ r1 ]


renderGridBg : Float -> Gwh -> Shape
renderGridBg gcs (Gwh (I2 gw gh)) =
    rectangle "lightyellow" (toFloat (gw + 1) * gcs) (toFloat (gh + 1) * gcs)


renderGCE : Float -> GCE -> Shape
renderGCE gcs (GCE (I2 x y) rc) =
    case rc of
        REmpty ->
            group []

        RWater isDown ->
            let
                rFact =
                    if isDown then
                        0.1

                    else
                        0.2
            in
            group
                [ ellipse1 "dodgerblue" (gcs * rFact)
                ]
                |> move (toFloat x * gcs) (toFloat y * gcs)


ellipse1 a b =
    ellipse a b b


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
    = Cwh F2


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
    ( M (flags.bs |> ffFromTuple |> Cwh) (Mxy 0 0) grid
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
update message ((M ((Cwh (F2 cw ch)) as cwh) mxy g) as model) =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotBS w h ->
            ( M (I2 w h |> iiToFloat |> Cwh) mxy g, Cmd.none )

        OnCMM x y ->
            ( M cwh (Mxy (x - cw * 0.5) (y - ch * 0.5)) g, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Browser.Events.onResize GotBS ]



-- View


type alias HM =
    Html Msg


view : Model -> Html Msg
view (M ((Cwh (F2 cw ch)) as cwh) mxy g) =
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


polyLine : String -> Float -> List ( Float, Float ) -> Shape
polyLine c sw pts =
    PolyLine sw pts |> Shape c [] initialTransform


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
    | PolyLine Float (List ( Float, Float ))


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
        PolyLine sw pts ->
            Svg.polyline
                [ SA.stroke c
                , TA.class cs
                , Px.strokeWidth sw
                , TA.points pts
                ]
                []

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
