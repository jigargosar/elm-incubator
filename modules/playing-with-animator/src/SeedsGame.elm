module SeedsGame exposing (main)

import Browser exposing (Document)
import Draw exposing (rect)
import Grid
import Html exposing (Html, text)
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
            [ rect "#ffc973" screenWidth screenHeight [] ]
        ]



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
