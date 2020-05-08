module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)



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
    Document "2048 Animated"
        [ div [ class "pa3 measure center" ]
            [ div [ class "pa3 f3" ] [ text "Play 2048" ]
            , viewGrid initialGrid
            ]
        ]


type alias Pos =
    ( Int, Int )


type alias Tile =
    { id : String
    , num : Int
    , pos : Pos
    }


initialGrid =
    [ [ 0, 0, 0, 0 ]
    , [ 0, 2, 0, 0 ]
    , [ 0, 0, 4, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


viewGrid =
    List.map viewRow
        >> div [ class "pa3 dib code f2 debug" ]


viewRow =
    List.map viewCell
        >> div [ class "flex" ]


viewCell num =
    div
        [ style "width" "100px"
        , style "height" "100px"
        , class "flex justify-center items-center"
        ]
        [ text (String.fromInt num) ]



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
