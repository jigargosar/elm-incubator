module Main exposing (main)

import Board
import Browser exposing (Document)
import Cons exposing (Cons)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Keyed
import IncId exposing (IncId)
import IntPos exposing (IntPos)
import Process
import Set
import Task
import Tuple exposing (first, mapBoth, mapFirst, second)


viewCellGrid : Board.CellGrid -> HM
viewCellGrid cellGrid =
    div
        [ class "pa3 code f2 debug" ]
        [ Html.Keyed.node "div"
            [ style "width" "400px"
            , style "height" "400px"
            ]
            (viewKeyedCells cellGrid)
        ]


viewKeyedCells : Board.CellGrid -> List ( String, HM )
viewKeyedCells cellGrid =
    let
        { generatedIds, merged } =
            cellGrid

        { entries } =
            Board.info cellGrid

        idToAnim : IncId -> TileAnim
        idToAnim id =
            if List.member id generatedIds then
                Generated

            else
                Existing

        cellViewList : List ( IncId, HM )
        cellViewList =
            entries
                |> List.map
                    (\( pos, cell ) ->
                        ( cell.id
                        , renderTile pos cell.num (idToAnim cell.id)
                        )
                    )

        mergedCellViewList : List ( IncId, HM )
        mergedCellViewList =
            List.map
                (\( pos, cell ) ->
                    ( cell.id
                    , renderTile pos cell.num Merged
                    )
                )
                merged
    in
    (cellViewList ++ mergedCellViewList)
        |> List.sortBy (first >> IncId.toInt)
        |> List.map (mapFirst IncId.toString)



-- Model


type alias Model =
    { tileListCons : Cons TileList
    , cellGrid : Board.CellGrid
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { tileListCons = initialTileListCons
      , cellGrid = Board.initialCellGrid
      }
    , Cmd.batch [ stepTiles, stepCellGrid ]
    )


stepTiles =
    Process.sleep 2000 |> Task.perform (always StepTiles)


stepCellGrid =
    Process.sleep 2000 |> Task.perform (always StepCellGrid)



-- Update


type Msg
    = NoOp
    | StepTiles
    | StepCellGrid


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        StepTiles ->
            case Cons.fromTail model.tileListCons of
                Nothing ->
                    ( { model | tileListCons = initialTileListCons }
                    , stepTiles
                    )

                Just gridCons ->
                    ( { model | tileListCons = gridCons }
                    , stepTiles
                    )

        StepCellGrid ->
            ( { model | cellGrid = Board.updateCellGrid model.cellGrid }, stepCellGrid )


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
            , renderTileListGrid (Cons.head model.tileListCons)
            , viewCellGrid model.cellGrid
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



-- TILE LIST


type alias TileList =
    List Tile


initialTileListCons : Cons TileList
initialTileListCons =
    let
        initTile : String -> Int -> IntPos -> TileAnim -> Tile
        initTile id num pos anim =
            { id = id
            , num = num
            , pos = pos
            , anim = anim
            , removed = False
            }

        initialTileList : TileList
        initialTileList =
            [ initTile "a" 2 ( 1, 1 ) Existing
            , initTile "b" 4 ( 2, 2 ) Existing
            ]

        restTileList : List TileList
        restTileList =
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

        reducer : TileList -> ( TileList, List TileList ) -> ( TileList, List TileList )
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
                |> (second >> List.reverse)
    in
    Cons.init initialTileList updatedRestTileList


renderTileListGrid : TileList -> HM
renderTileListGrid tiles =
    div
        [ class "pa3 code f2 debug" ]
        [ Html.Keyed.node "div"
            [ style "width" "400px"
            , style "height" "400px"
            ]
            (List.map viewKeyedTile tiles)
        ]


viewKeyedTile : Tile -> ( String, HM )
viewKeyedTile tile =
    ( tile.id
    , if tile.removed then
        text ""

      else
        renderTile tile.pos tile.num tile.anim
    )


renderTile : IntPos -> Int -> TileAnim -> HM
renderTile pos num anim =
    div
        [ style "width" "100px"
        , style "height" "100px"
        , class "absolute flex justify-center items-center"
        , style "transform" (renderTileTransform pos)
        , style "transition" "transform 500ms"
        , style "outline" "none"
        ]
        [ div
            [ style "width" "100px"
            , style "height" "100px"
            , style "background-color" "rgba(255,255,255,0.9)"
            , class "flex justify-center items-center"
            , case anim of
                Generated ->
                    class "animate__animated  animate__zoomIn animate__delay-2s "

                Merged ->
                    --class "animate__animated  animate__bounceIn "
                    class "animate__animated  animate__zoomIn "

                Existing ->
                    class ""
            ]
            [ text (String.fromInt num) ]
        ]


renderTileTransform pos =
    let
        postPartToPx n =
            String.fromInt (n * 100) ++ "px"

        ( sx, sy ) =
            pos
                |> mapBoth postPartToPx postPartToPx
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
