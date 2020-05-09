module Main exposing (main)

import Browser exposing (Document)
import Cons exposing (Cons)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Keyed
import IntPos exposing (IntPos)
import Process
import Set
import Task



-- Model


type alias Model =
    { gridCons : Cons GridModel
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { gridCons = initialGridModelCons
      }
    , stepTiles
    )


stepTiles =
    Process.sleep 2000 |> Task.perform (always StepTiles)



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
                    ( { model
                        | gridCons = gridCons
                      }
                    , stepTiles
                    )


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
            , renderGridModel (Cons.head model.gridCons)
            ]
        ]



-- Tile


type alias Tile =
    { id : String
    , num : Int
    , pos : IntPos
    , anim : TileAnim
    , removed : Bool
    }


type TileAnim
    = Generated
    | Merged
    | Existing



-- Grid


type alias GridModel =
    List Tile


gridFromTiles : List Tile -> GridModel
gridFromTiles =
    identity


initialGridModelCons : Cons GridModel
initialGridModelCons =
    let
        initTile : String -> Int -> IntPos -> TileAnim -> Tile
        initTile id num pos anim =
            { id = id
            , num = num
            , pos = pos
            , anim = anim
            , removed = False
            }

        initialTileList : GridModel
        initialTileList =
            gridFromTiles
                [ initTile "a" 2 ( 1, 1 ) Existing
                , initTile "b" 4 ( 2, 2 ) Existing
                ]

        restTileList : List GridModel
        restTileList =
            List.map gridFromTiles
                [ -- Right
                  [ initTile "a" 2 ( 3, 1 ) Existing
                  , initTile "b" 4 ( 3, 2 ) Existing
                  , initTile "c" 2 ( 2, 1 ) Generated
                  ]
                , -- Left
                  [ initTile "a" 2 ( 0, 1 ) Existing
                  , initTile "b" 4 ( 0, 2 ) Existing
                  , initTile "c" 2 ( 0, 1 ) Existing
                  , initTile "d" 4 ( 0, 1 ) Merged
                  , initTile "e" 2 ( 1, 1 ) Generated
                  ]
                , -- Up
                  [ initTile "b" 4 ( 0, 0 ) Existing
                  , initTile "d" 4 ( 0, 0 ) Existing
                  , initTile "e" 2 ( 1, 0 ) Existing
                  , initTile "f" 8 ( 0, 0 ) Merged
                  , initTile "g" 4 ( 1, 1 ) Generated
                  ]

                -- Right
                , [ initTile "e" 2 ( 3, 0 ) Existing
                  , initTile "f" 8 ( 2, 0 ) Existing
                  , initTile "g" 4 ( 3, 1 ) Existing
                  ]

                -- Clear
                , []
                ]

        reducer : GridModel -> ( GridModel, List GridModel ) -> ( GridModel, List GridModel )
        reducer tiles ( previousTiles, lists ) =
            let
                newIdSet =
                    List.map .id tiles |> Set.fromList

                tilesWithReadyForRemoval =
                    previousTiles
                        |> List.filterMap
                            (\t ->
                                if t.removed || Set.member t.id newIdSet then
                                    Nothing

                                else
                                    Just { t | removed = True }
                            )

                finalTiles =
                    tiles
                        ++ tilesWithReadyForRemoval
                        |> List.sortBy .id
            in
            ( finalTiles, finalTiles :: lists )

        updatedRestTileList =
            List.foldl reducer ( initialTileList, [] ) restTileList
                |> (Tuple.second >> List.reverse)
    in
    Cons.init initialTileList updatedRestTileList


renderGridModel : GridModel -> HM
renderGridModel tiles =
    div
        [ class "pa3 code f2 debug"
        ]
        [ Html.Keyed.node "div"
            [ style "width" "400px"
            , style "height" "400px"
            ]
            (List.map renderKeyedTile tiles)
        ]


renderKeyedTile : Tile -> ( String, HM )
renderKeyedTile tile =
    ( tile.id
    , if tile.removed then
        text ""

      else
        viewTile tile
    )


viewTile : Tile -> HM
viewTile tile =
    div
        [ style "width" "100px"
        , style "height" "100px"
        , class "absolute flex justify-center items-center"
        , style "transform" (renderTileTransform tile)
        , style "transition" "transform 500ms"
        , style "outline" "none"
        ]
        [ div
            [ style "width" "100px"
            , style "height" "100px"
            , style "background-color" "rgba(255,255,255,0.9)"
            , class "flex justify-center items-center"
            , case tile.anim of
                Generated ->
                    class "animate__animated  animate__zoomIn animate__delay-2s "

                Merged ->
                    --class "animate__animated  animate__bounceIn "
                    class "animate__animated  animate__zoomIn "

                Existing ->
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
