module SeedsGame exposing (main)

import Basics.Extra exposing (uncurry)
import Browser exposing (Document)
import Draw exposing (circle, rect)
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
    Grid.toList g
        |> List.map (uncurry renderGridCellAt)
        |> group


group =
    Svg.g []


renderGridCellAt : GIdx -> Cell -> Svg.Svg msg
renderGridCellAt gIdx cell =
    case cell of
        Water ->
            circle "dodgerblue" (gridCellWidth * 0.3) []



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
