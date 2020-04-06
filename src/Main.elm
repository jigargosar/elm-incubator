module Main exposing (main)

-- Browser.Element Scaffold

import Basics.Extra exposing (flip, uncurry)
import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Json.Decode as JD exposing (Decoder)
import List.Extra exposing (scanl)
import Svg
import Svg.Attributes as SA
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


ffRound (F2 a b) =
    I2 (round a) (round b)



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


iiIndicesOf : I2 -> List I2
iiIndicesOf (I2 w h) =
    let
        fn : Int -> List I2
        fn x =
            List.range 0 (h - 1) |> List.map (I2 x)
    in
    List.range 0 (w - 1) |> List.concatMap fn


iiToPair : I2 -> ( Int, Int )
iiToPair =
    iiApply2 Tuple.pair


iiIsValidIdxOf : I2 -> I2 -> Bool
iiIsValidIdxOf (I2 w h) (I2 x y) =
    x >= 0 && x < w && y >= 0 && y < h


iiValidIdxOf : I2 -> I2 -> Maybe I2
iiValidIdxOf wh idx =
    if iiIsValidIdxOf wh idx then
        Just idx

    else
        Nothing



-- I2Dict


type IIDict a
    = IIDict (Dict ( Int, Int ) a)


iidFromList : List ( I2, a ) -> IIDict a
iidFromList =
    List.map (Tuple.mapFirst iiToPair) >> Dict.fromList >> IIDict



--iidInsert : II -> a -> IIDict a -> IIDict a
--iidInsert ii a (IIDict d) =
--    IIDict (Dict.insert (iiToPair ii) a d)
--iidGet : I2 -> IIDict a -> Maybe a
--iidGet ii (IIDict d) =
--    Dict.get (iiToPair ii) d


iidGetEntry : I2 -> IIDict a -> Maybe ( I2, a )
iidGetEntry ii (IIDict d) =
    Dict.get (iiToPair ii) d |> Maybe.map (Tuple.pair ii)


iidToList : IIDict a -> List ( I2, a )
iidToList (IIDict d) =
    Dict.toList d |> List.map (Tuple.mapFirst (uncurry I2))



--iidGetAll : List I2 -> IIDict a -> Maybe (List a)
--iidGetAll iiKeys iid =
--    List.map (flip iidGet iid) iiKeys
--        |> Maybe.Extra.combine
-- Grid


type Grid
    = G Gwh (IIDict Cell) (List I2)


type Cell
    = Water
    | Seed


type Gwh
    = Gwh I2


initialGrid : Grid
initialGrid =
    let
        ( w, h ) =
            ( 8, 8 )

        gd =
            iiIndicesOf (I2 w h)
                |> List.map (\xy -> ( xy, Water ))
                |> List.Extra.updateIf (Tuple.first >> flip List.member seedIndices)
                    (Tuple.mapSecond (always Seed))
                |> iidFromList

        seedIndices =
            scanl (<|) (I2 1 4) [ iiRight, iiRight, iiDown, iiDown ]

        conIdxStack =
            -- scanl (<|) (I2 2 2) [ iiRight, iiRight, iiDown, iiDown ]
            --    |> List.reverse
            seedIndices
    in
    G (Gwh (I2 w h)) gd conIdxStack



-- GRID CONTEXT


type alias GCtx =
    { cs : Float
    , gwh : Gwh
    , dxy : F2
    , top : Float
    , right : Float
    , bottom : Float
    , left : Float
    }


toGCtx : Cwh -> Grid -> GCtx
toGCtx cwh (G gwh _ _) =
    toGCtxHelp cwh gwh


toGCtxHelp : Cwh -> Gwh -> GCtx
toGCtxHelp cwh ((Gwh wh) as gwh) =
    let
        getGcs : Cwh -> Gwh -> Float
        getGcs (Cwh (F2 cw ch)) (Gwh (I2 gw gh)) =
            min (cw * (1 / toFloat (gw + 1))) (ch * (1 / toFloat (gh + 1)))
                * 0.8
                |> clamp 10 50
    in
    let
        getGDxy : Float -> Gwh -> F2
        getGDxy gcs_ (Gwh wh_) =
            let
                (F2 w h) =
                    iiToFloat wh_

                dx =
                    ((w * gcs_) - gcs_) * -0.5

                dy =
                    ((h * gcs_) - gcs_) * -0.5
            in
            F2 dx dy
    in
    let
        gcs =
            getGcs cwh gwh

        (F2 w h) =
            iiToFloat wh

        dxy =
            getGDxy gcs gwh
    in
    { cs = gcs
    , gwh = gwh
    , dxy = dxy
    , top = -h / 2
    , right = w / 2
    , bottom = h / 2
    , left = -w / 2
    }



-- GRID UPDATE


canCellStartConnection : Cell -> Bool
canCellStartConnection cell =
    case cell of
        Water ->
            True

        Seed ->
            True


updateGridOnMouseClick : GCtx -> Mxy -> Grid -> Grid
updateGridOnMouseClick ctx (Mxy mx my) ((G gwh gd conI2Stack) as g) =
    let
        maybeClickedEntry =
            canvasToGIdx ctx (F2 mx my)
                |> Maybe.andThen (flip iidGetEntry gd)
    in
    case conI2Stack of
        [] ->
            case maybeClickedEntry of
                Just ( idx, cell ) ->
                    if canCellStartConnection cell then
                        G gwh gd [ idx ]

                    else
                        g

                Nothing ->
                    g

        _ :: _ :: _ ->
            G gwh gd []

        _ ->
            g


updateGridOnMouseMove : GCtx -> Mxy -> Grid -> Grid
updateGridOnMouseMove ctx (Mxy mx my) ((G gwh gd conI2Stack) as g) =
    let
        func : ( I2, Cell ) -> Maybe ( I2, Cell ) -> Grid
        func ( gIdx, cell ) mbLstEntry =
            if List.member gIdx conI2Stack then
                if List.Extra.elemIndex gIdx conI2Stack == Just 1 then
                    G gwh gd (List.drop 1 conI2Stack)

                else
                    g

            else
                case mbLstEntry of
                    Nothing ->
                        g

                    Just ( lstIdx, lstCell ) ->
                        if areAdjacent lstIdx gIdx && cell == lstCell then
                            G gwh gd (gIdx :: conI2Stack)

                        else
                            g
    in
    if List.isEmpty conI2Stack then
        g

    else
        Maybe.map2 func
            (canvasToGIdx ctx (F2 mx my)
                |> Maybe.andThen (flip iidGetEntry gd)
            )
            (conI2Stack
                |> List.head
                |> Maybe.map (flip iidGetEntry gd)
            )
            |> Maybe.withDefault g


areAdjacent (I2 x1 y1) (I2 x2 y2) =
    (abs (x1 - x2) == 1 && y1 == y2)
        || (abs (y1 - y2) == 1 && x1 == x2)



-- GRID VIEW


renderGrid : Cwh -> Mxy -> Grid -> HM
renderGrid cwh mxy g =
    toGridVM g |> renderGridVM (toGCtx cwh g) mxy



-- GRID VIEW MODEL


type GridVM
    = GV Gwh (List GCE) (List I2) (Maybe GCE)


type GCE
    = GCE I2 RCell


type RCell
    = RWater Bool
    | RSeed Bool


gceIdxEq expected (GCE actual _) =
    actual == expected


toGridVM : Grid -> GridVM
toGridVM (G gwh gd conI2Stack) =
    let
        toGCE ( xy, c ) =
            GCE xy
                (case c of
                    Water ->
                        RWater (List.member xy conI2Stack)

                    Seed ->
                        RSeed (List.member xy conI2Stack)
                )

        ls : List GCE
        ls =
            iidToList gd |> List.map toGCE

        ( mbLastGCE, gceList ) =
            case conI2Stack of
                idx :: _ ->
                    ls
                        |> List.Extra.select
                        |> List.Extra.find (Tuple.first >> gceIdxEq idx)
                        |> Maybe.map (Tuple.mapFirst Just)
                        |> Maybe.withDefault ( Nothing, ls )

                [] ->
                    ( Nothing, ls )
    in
    GV gwh gceList (List.reverse conI2Stack) mbLastGCE



-- RENDER GRID VIEW MODEL


renderGridVM : GCtx -> Mxy -> GridVM -> HM
renderGridVM ctx (Mxy mx my) (GV gwh gceList conIndices mbLastGCE) =
    let
        gcs =
            ctx.cs

        renderGridCellEntries : List GCE -> Shape
        renderGridCellEntries l =
            l
                |> List.map (renderGCE gcs)
                |> group
                |> moveF2 ctx.dxy

        renderMouseConnection : GCE -> Shape
        renderMouseConnection (GCE xy _) =
            let
                (F2 x1 y1) =
                    gIdxToCanvas ctx xy
            in
            group
                [ connectionPolyLine gcs [ ( x1, y1 ), ( mx, my ) ]
                ]

        renderCellConnections : Shape
        renderCellConnections =
            let
                idxToPt (I2 a b) =
                    ( toFloat a * gcs, toFloat b * gcs )
            in
            group [ connectionPolyLine gcs (List.map idxToPt conIndices) ]
                |> moveF2 ctx.dxy

        renderLastCellAndConnectionToMouse =
            case mbLastGCE of
                Nothing ->
                    group []

                Just lastGCE ->
                    group [ renderMouseConnection lastGCE, renderGridCellEntries [ lastGCE ] ]
    in
    group
        [ renderGridBg gcs gwh
        , renderCellConnections
        , renderGridCellEntries gceList
        , renderLastCellAndConnectionToMouse
        , renderPointer (gcs * 0.25) mx my
        ]
        |> draw


canvasToGIdx : GCtx -> F2 -> Maybe I2
canvasToGIdx ctx (F2 x y) =
    let
        (F2 dx dy) =
            ctx.dxy

        gcs =
            ctx.cs
    in
    F2 ((x - dx) / gcs) ((y - dy) / gcs)
        |> ffRound
        |> validGIdx ctx.gwh


validGIdx : Gwh -> I2 -> Maybe I2
validGIdx (Gwh wh) idx =
    iiValidIdxOf wh idx


gIdxToCanvas : GCtx -> I2 -> F2
gIdxToCanvas ctx xy =
    let
        (F2 x y) =
            iiToFloat xy

        (F2 dx dy) =
            ctx.dxy

        gcs =
            ctx.cs
    in
    F2 (x * gcs + dx) (y * gcs + dy)


moveF2 : F2 -> Shape -> Shape
moveF2 (F2 dx dy) =
    move dx dy


connectionPolyLine : Float -> List ( Float, Float ) -> Shape
connectionPolyLine gcs =
    polyLine "green" (gcs * 0.03)


renderGridBg : Float -> Gwh -> Shape
renderGridBg gcs (Gwh (I2 gw gh)) =
    rectangle "lightyellow" (toFloat (gw + 1) * gcs) (toFloat (gh + 1) * gcs)


renderGCE : Float -> GCE -> Shape
renderGCE gcs (GCE (I2 x y) rc) =
    case rc of
        RWater isDown ->
            let
                rFact =
                    if isDown then
                        0.1

                    else
                        0.2
            in
            group
                [ circle "dodgerblue" (gcs * rFact)
                ]
                |> move (toFloat x * gcs) (toFloat y * gcs)

        RSeed isDown ->
            let
                rFact =
                    if isDown then
                        0.1

                    else
                        0.2
            in
            group
                [ circle "brown" (gcs * rFact)
                ]
                |> move (toFloat x * gcs) (toFloat y * gcs)


circle : String -> Float -> Shape
circle a b =
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
            initialGrid
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
    | OnMouseMove Float Float
    | OnClick Float Float


update : Msg -> Model -> ( Model, Cmd Msg )
update message ((M ((Cwh (F2 cw ch)) as cwh) mxy g) as model) =
    let
        ctx =
            toGCtx cwh g
    in
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotBS w h ->
            ( M (I2 w h |> iiToFloat |> Cwh) mxy g, Cmd.none )

        OnMouseMove x y ->
            let
                newMxy =
                    Mxy (x - cw * 0.5) (y - ch * 0.5)

                newGrid =
                    updateGridOnMouseMove ctx newMxy g
            in
            ( M cwh newMxy newGrid, Cmd.none )

        OnClick x y ->
            let
                newMxy =
                    Mxy (x - cw * 0.5) (y - ch * 0.5)

                newGrid =
                    updateGridOnMouseClick ctx newMxy g
            in
            ( M cwh newMxy newGrid, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize GotBS
        , Browser.Events.onMouseMove pageMouseMoveDecoder
        , Browser.Events.onClick (pageXYDecoder OnClick)
        ]


pageXYDecoder : (Float -> Float -> msg) -> Decoder msg
pageXYDecoder tag =
    JD.map2 tag
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)


pageMouseMoveDecoder : Decoder Msg
pageMouseMoveDecoder =
    pageXYDecoder OnMouseMove



-- View


type alias HM =
    Html Msg


view : Model -> Html Msg
view (M ((Cwh (F2 cw ch)) as cwh) mxy g) =
    div
        [ class "fixed absolute--fill"
        ]
        [ Svg.svg
            [ TA.viewBox (cw * -0.5) (ch * -0.5) cw ch
            , SA.width "100%"
            , SA.height "100%"

            --, TA.preserveAspectRatio (TT.Align TT.ScaleMid TT.ScaleMid) TT.Meet
            , style "background-color" "rgba(183, 169, 255)"
            ]
            [ draw (rectangle "rgba(153, 248, 255)" cw ch)
            , draw (rectangle "rgba(183, 169, 255)" cw ch)
            , renderGrid cwh mxy g
            ]
        ]


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
