module SeedsGame exposing (main)

import Basics.Extra exposing (uncurry)
import Browser exposing (Document)
import Draw exposing (circle, move, rect)
import Grid exposing (GIdx)
import Svg exposing (svg)
import TypedSvg.Attributes exposing (viewBox)
import TypedSvg.Attributes.InPx exposing (height, width)



-- Model


type Model
    = M Grid


type alias Grid =
    Grid.Grid Cell


initialGrid : Grid
initialGrid =
    Grid.init 7 5 (\_ -> Water)


type Cell
    = Water


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( M initialGrid
    , Cmd.none
    )



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


type alias DM =
    Document Msg


screenWidth =
    600


screenHeight =
    800


screenLeft =
    screenWidth * -0.5


screenTop =
    screenHeight * -0.5


view : Model -> DM
view (M g) =
    Document "SeedsGame"
        [ svg
            [ viewBox screenLeft screenTop screenWidth screenHeight
            , width screenWidth
            , height screenHeight
            ]
            [ rect "#ffc973" screenWidth screenHeight []
            , renderGrid g
            ]
        ]


type alias GCtx =
    { cw : Float
    , dx : Float
    , dy : Float
    }


toGCtx : Grid -> GCtx
toGCtx g =
    let
        gridCellWidth =
            50

        ( gridWidth, gridHeight ) =
            Grid.wh g |> Tuple.mapBoth (toFloat >> (*) gridCellWidth) (toFloat >> (*) gridCellWidth)

        dx =
            (gridWidth - gridCellWidth) * -0.5

        dy =
            (gridHeight - gridCellWidth) * -0.5
    in
    { cw = gridCellWidth
    , dx = dx
    , dy = dy
    }


renderGrid : Grid -> Svg.Svg msg
renderGrid g =
    let
        ctx =
            toGCtx g

        renderCellAt ( gIdx, cell ) =
            renderCell ctx gIdx cell
    in
    Grid.toList g
        |> List.map renderCellAt
        |> group


group =
    Svg.g []


moveToGIdx : GCtx -> GIdx -> Draw.Op
moveToGIdx ctx gIdx =
    uncurry move (gIdxToXY ctx gIdx)


gIdxToXY : GCtx -> GIdx -> ( Float, Float )
gIdxToXY { cw, dx, dy } ( xi, yi ) =
    ( toFloat xi * cw + dx, toFloat yi * cw + dy )


renderCell : GCtx -> GIdx -> Cell -> Svg.Svg msg
renderCell ({ cw } as ctx) gIdx cell =
    let
        mv =
            moveToGIdx ctx gIdx
    in
    case cell of
        Water ->
            circle "dodgerblue" (cw * 0.25) [ mv ]



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
