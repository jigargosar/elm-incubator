module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Keyed



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
            , viewTilesGrid initialTiles
            ]
        ]


type alias Pos =
    ( Int, Int )


type alias Tile =
    { id : String
    , num : Int
    , pos : Pos
    }


initialTiles =
    [ Tile "a" 2 ( 1, 1 )
    , Tile "b" 4 ( 2, 2 )
    ]


viewTilesGrid tiles =
    div
        [ class "pa3 code f2 debug"
        ]
        [ Html.Keyed.node "div"
            [ style "width" "400px"
            , style "height" "400px"
            ]
            (List.map viewKeyedTile tiles)
        ]


viewKeyedTile tile =
    ( tile.id, viewTile tile )


viewTile tile =
    div
        [ style "width" "100px"
        , style "height" "100px"
        , style "background-color" "rgba(255,255,255,0.9)"
        , class "absolute flex justify-center items-center"
        , style "transform" (renderTileTransform tile)
        ]
        [ text (String.fromInt tile.num) ]


renderTileTransform tile =
    let
        postPartToPx n =
            String.fromInt (n * 100) ++ "px"

        ( sx, sy ) =
            tile.pos
                |> Tuple.mapBoth postPartToPx postPartToPx
    in
    [ "translate(", sx, ",", sy, ")" ]
        |> String.join " "



---- Grid
--
--initialGrid =
--    [ [ 0, 0, 0, 0 ]
--    , [ 0, 2, 0, 0 ]
--    , [ 0, 0, 4, 0 ]
--    , [ 0, 0, 0, 0 ]
--    ]
--
--
--viewGrid =
--    List.map viewRow
--        >> div [ class "pa3 dib code f2 debug" ]
--
--
--viewRow =
--    List.map viewCell
--        >> div [ class "flex" ]
--
--
--viewCell num =
--    div
--        [ style "width" "100px"
--        , style "height" "100px"
--        , class "flex justify-center items-center"
--        ]
--        [ text (String.fromInt num) ]
--
-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
