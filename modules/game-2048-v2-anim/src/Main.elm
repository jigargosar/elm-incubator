module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Keyed
import Process
import Task



-- Model


type alias Model =
    { tiles : List Tile
    , tilesLists : List (List Tile)
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { tiles = initialTiles
      , tilesLists = tilesLists
      }
    , stepTiles
    )


stepTiles =
    Process.sleep 750 |> Task.perform (always StepTiles)



-- Update


type Msg
    = NoOp
    | StepTiles


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        StepTiles ->
            case model.tilesLists of
                [] ->
                    init ()

                h :: t ->
                    ( { model | tiles = h, tilesLists = t }, stepTiles )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


type alias DM =
    Document Msg


type alias HM =
    Html Msg


view : Model -> DM
view model =
    Document "2048 Animated"
        [ div [ class "pa3 measure center" ]
            [ div [ class "pa3 f3" ] [ text "Play 2048" ]
            , viewTilesGrid model.tiles
            ]
        ]


type alias Pos =
    ( Int, Int )


type alias Tile =
    { id : String
    , num : Int
    , pos : Pos
    , popIn : Bool
    }


initialTiles =
    [ Tile "a" 2 ( 1, 1 ) False
    , Tile "b" 4 ( 2, 2 ) False
    ]


tilesLists =
    [ [ Tile "a" 2 ( 3, 1 ) False
      , Tile "b" 4 ( 3, 2 ) False
      ]
    , [ Tile "a" 2 ( 3, 1 ) False
      , Tile "b" 4 ( 3, 2 ) False
      ]
    , [ Tile "a" 2 ( 3, 1 ) False
      , Tile "b" 4 ( 3, 2 ) False
      , Tile "c" 2 ( 2, 1 ) True
      ]
    , [ Tile "a" 2 ( 0, 1 ) False
      , Tile "b" 4 ( 0, 2 ) False
      , Tile "c" 2 ( 0, 1 ) False
      , Tile "d" 4 ( 0, 1 ) True
      ]
    , [ Tile "b" 4 ( 0, 2 ) False
      , Tile "d" 4 ( 0, 1 ) True
      , Tile "e" 2 ( 1, 1 ) True
      ]
    , [ Tile "b" 4 ( 0, 0 ) False
      , Tile "d" 4 ( 0, 0 ) False
      , Tile "e" 2 ( 1, 0 ) False
      , Tile "f" 8 ( 0, 0 ) True
      ]
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


viewTile : Tile -> HM
viewTile tile =
    div
        [ style "width" "100px"
        , style "height" "100px"
        , style "background-color" "rgba(255,255,255,0.9)"
        , class "absolute flex justify-center items-center"
        , style "transform" (renderTileTransform tile)
        , style "transition" "transform 750ms"
        ]
        [ div
            [ style "width" "100px"
            , style "height" "100px"
            , class "flex justify-center items-center"
            , if tile.popIn then
                class "animated bounceIn"

              else
                class ""
            ]
            [ text (String.fromInt tile.num) ]
        ]


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
