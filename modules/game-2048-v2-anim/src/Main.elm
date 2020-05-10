module Main exposing (main)

import Browser exposing (Document)
import Cons exposing (Cons)
import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Keyed
import IncId exposing (IncId)
import IntPos exposing (IntPos)
import IntSize
import PosDict exposing (PosDict)
import Process
import Set
import Task



-- Cell


type Cell
    = Cell IncId Int
    | Empty


newCell : Int -> IncId.Generator -> ( Cell, IncId.Generator )
newCell num generator =
    let
        initCell id =
            Cell id num
    in
    IncId.new generator
        |> Tuple.mapFirst initCell



-- Cell Grid


type alias CellGrid =
    { idGenerator : IncId.Generator
    , dict : PosDict Cell
    }


size =
    IntSize.new 4 4


initialCellGrid : CellGrid
initialCellGrid =
    let
        idGen0 =
            IncId.newGenerator

        ( cell1, idGen1 ) =
            newCell 2 idGen0

        ( cell2, idGen2 ) =
            newCell 4 idGen1
    in
    { idGenerator = idGen2
    , dict =
        PosDict.fill Empty size
            |> Dict.insert ( 1, 1 ) cell1
            |> Dict.insert ( 2, 2 ) cell2
    }


viewCellGrid : CellGrid -> HM
viewCellGrid cellGrid =
    div
        [ class "pa3 code f2 debug" ]
        [ Html.Keyed.node "div"
            [ style "width" "400px"
            , style "height" "400px"
            ]
            (viewKeyedCells cellGrid.dict)
        ]


viewKeyedCells : PosDict Cell -> List ( String, HM )
viewKeyedCells dict =
    let
        l1 =
            dict
                |> Dict.toList
                |> List.filterMap
                    (\( pos, cell ) ->
                        case cell of
                            Cell id num ->
                                Just ( id, renderTile pos num Existing )

                            Empty ->
                                Nothing
                    )
    in
    l1
        |> List.sortBy (Tuple.first >> IncId.toInt)
        |> List.map (Tuple.mapFirst IncId.toString)



-- Model


type alias Model =
    { tileListCons : Cons TileList
    , cellGrid : CellGrid
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { tileListCons = initialTileListCons
      , cellGrid = initialCellGrid
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
            case Cons.fromTail model.tileListCons of
                Nothing ->
                    init ()

                Just gridCons ->
                    ( { model | tileListCons = gridCons }
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
                |> (Tuple.second >> List.reverse)
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
