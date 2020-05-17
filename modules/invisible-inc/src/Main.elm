module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
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
        , div [ class "flex justify-center" ] [ viewGrid ]
        ]


viewGrid =
    let
        gridSize =
            { width = 10
            , height = 15
            }
    in
    positionsOf gridSize
        |> List.map viewPosition
        |> div
            [ widthPx (cellWidth * gridSize.width)
            , heightPx (cellWidth * gridSize.height)
            ]


cellWidth =
    50


widthPx n =
    style "width" (String.fromInt n ++ "px")


heightPx n =
    style "height" (String.fromInt n ++ "px")


viewPosition pos =
    div
        [ widthPx cellWidth
        , heightPx cellWidth
        , style "transform" (renderCellTransform pos)
        , class "absolute pa1"
        ]
        [ div [ class "w-100 h-100 br3 bg-pink" ] [] ]


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
