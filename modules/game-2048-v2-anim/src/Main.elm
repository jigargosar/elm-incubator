module Main exposing (main)

import Browser exposing (Document)
import Cons exposing (Cons)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Keyed
import IntPos exposing (IntPos)
import Process
import Task



-- Model


type alias Model =
    { gridCons : Cons GridModel
    , gridViewModel : GridViewModel
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { gridCons = initialGridModelCons
      , gridViewModel =
            Cons.head initialGridModelCons
                |> initGridViewModel
      }
    , stepTiles
    )


stepTiles =
    Process.sleep 1500 |> Task.perform (always StepTiles)



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
            case Cons.fromTail model.gridCons of
                Nothing ->
                    init ()

                Just gridCons ->
                    ( { model | gridCons = gridCons }, stepTiles )


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
            , viewTilesListAsGrid (Cons.head model.gridCons)
            ]
        ]



-- Tile


type alias Tile =
    { id : String
    , num : Int
    , pos : IntPos
    , foo : TileFoo
    }


type TileFoo
    = Generated
    | Merged
    | None


type TileView
    = TileVisible Tile
    | TileToBeRemoved String


type alias GridViewModel =
    List TileView


initGridViewModel : GridModel -> GridViewModel
initGridViewModel tiles =
    List.map TileVisible tiles



-- Grid


type alias GridModel =
    List Tile


gridFromTiles : List Tile -> GridModel
gridFromTiles =
    identity


initialGridModelCons : Cons GridModel
initialGridModelCons =
    let
        initialTileList : GridModel
        initialTileList =
            gridFromTiles
                [ Tile "a" 2 ( 1, 1 ) None
                , Tile "b" 4 ( 2, 2 ) None
                ]

        restTileList : List GridModel
        restTileList =
            List.map gridFromTiles
                [ -- Right
                  [ Tile "a" 2 ( 3, 1 ) None
                  , Tile "b" 4 ( 3, 2 ) None
                  , Tile "c" 2 ( 2, 1 ) Generated
                  ]
                , -- Left
                  [ Tile "a" 2 ( 0, 1 ) None
                  , Tile "b" 4 ( 0, 2 ) None
                  , Tile "c" 2 ( 0, 1 ) None
                  , Tile "d" 4 ( 0, 1 ) Merged
                  , Tile "e" 2 ( 1, 1 ) Generated
                  ]
                , -- Up
                  [ Tile "b" 4 ( 0, 0 ) None
                  , Tile "d" 4 ( 0, 0 ) None
                  , Tile "e" 2 ( 1, 0 ) None
                  , Tile "f" 8 ( 0, 0 ) Merged
                  , Tile "g" 4 ( 1, 1 ) Generated
                  ]

                -- Right
                , [ Tile "e" 2 ( 3, 0 ) None
                  , Tile "f" 8 ( 2, 0 ) None
                  , Tile "g" 4 ( 3, 1 ) None
                  ]

                -- Clear
                , []
                ]
    in
    Cons.init initialTileList restTileList


viewTilesListAsGrid : List Tile -> HM
viewTilesListAsGrid tiles =
    div
        [ class "pa3 code f2 debug"
        ]
        [ Html.Keyed.node "div"
            [ style "width" "400px"
            , style "height" "400px"
            ]
            (List.map viewKeyedTile tiles)
        ]


viewKeyedTile : Tile -> ( String, HM )
viewKeyedTile tile =
    ( tile.id, viewTile tile )


viewTile : Tile -> HM
viewTile tile =
    div
        [ style "width" "100px"
        , style "height" "100px"
        , class "absolute flex justify-center items-center"
        , style "transform" (renderTileTransform tile)
        , style "transition" "transform 750ms"
        , style "outline" "none"
        ]
        [ div
            [ style "width" "100px"
            , style "height" "100px"
            , style "background-color" "rgba(255,255,255,0.9)"
            , class "flex justify-center items-center"
            , case tile.foo of
                Generated ->
                    class "animate__animated  animate__zoomIn animate__delay-2s "

                Merged ->
                    class "animate__animated  animate__bounceIn "

                None ->
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



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
