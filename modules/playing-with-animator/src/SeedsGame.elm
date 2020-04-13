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


gridCellWidth =
    50


renderGrid : Grid -> Svg.Svg msg
renderGrid g =
    let
        renderCellAt ( gIdx, cell ) =
            renderCell g gIdx cell
    in
    Grid.toList g
        |> List.map renderCellAt
        |> group


gIdxToXY : GIdx -> Grid.Grid a -> ( Float, Float )
gIdxToXY ( xi, yi ) g =
    let
        ( gridWidth, gridHeight ) =
            Grid.wh g |> Tuple.mapBoth (toFloat >> (*) gridCellWidth) (toFloat >> (*) gridCellWidth)

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


group =
    Svg.g []


moveToGIdx g gIdx =
    uncurry move (gIdxToXY gIdx g)


renderCell : Grid -> GIdx -> Cell -> Svg.Svg msg
renderCell g gIdx cell =
    case cell of
        Water ->
            circle "dodgerblue" (gridCellWidth * 0.25) [ moveToGIdx g gIdx ]



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
