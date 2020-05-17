module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Tuple exposing (pair)



-- Model


type alias Model =
    {}


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( {}
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


view : Model -> DM
view _ =
    Document "Invisible Inc."
        [ text "Hello Invisible Inc."
        , viewGrid
        ]


viewGrid =
    let
        gridSize =
            { width = 10
            , height = 20
            }
    in
    positionsOf gridSize
        |> List.map viewPosition
        |> div []


cellWidth =
    50


viewPosition pos =
    div
        [ style "width" (String.fromInt cellWidth)
        , style "height" (String.fromInt cellWidth)
        , style "transform" (renderCellTransform pos)
        ]
        []


renderCellTransform ( x, y ) =
    "translate(" ++ String.fromInt (x * cellWidth) ++ "px, " ++ String.fromInt (y * cellWidth) ++ "px)"


type alias IntPos =
    ( Int, Int )


type alias IntSize =
    { width : Int
    , height : Int
    }


positionsOf : IntSize -> List IntPos
positionsOf s =
    List.range 0 (s.width - 1)
        |> List.concatMap (\x -> List.range 0 (s.height - 1) |> List.map (pair x))



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
