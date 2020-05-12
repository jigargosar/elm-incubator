module Main exposing (main)

import Board
import Browser exposing (Document)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Keyed
import IncId exposing (IncId)
import IntPos exposing (IntPos)
import Process
import Random
import Task
import Tuple exposing (..)



-- Model


type alias Model =
    { board : Board.Board
    , nextBoardMsg : Board.Msg
    , seed : Random.Seed
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { board = Board.init
      , nextBoardMsg = Board.SlideRight
      , seed = Random.initialSeed 0
      }
    , Cmd.batch [ stepCellGrid ]
    )


stepCellGrid =
    Process.sleep 2000 |> Task.perform (always StepCellGrid)



-- Update


type Msg
    = NoOp
    | StepCellGrid


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        StepCellGrid ->
            let
                ( nextBoardMsg, seed ) =
                    Random.step boardMsgGenerator model.seed
            in
            ( { model
                | board = Board.update model.nextBoardMsg model.board
                , nextBoardMsg = nextBoardMsg
                , seed = seed
              }
            , stepCellGrid
            )


boardMsgGenerator : Random.Generator Board.Msg
boardMsgGenerator =
    Random.uniform Board.SlideLeft
        [ Board.SlideRight, Board.SlideUp, Board.SlideDown ]


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
        [ div [ class "pa2 measure center" ]
            [ div [ class "pa2" ]
                [ div [] [ text (Debug.toString model.nextBoardMsg) ]
                , div [] [ text ("Score:" ++ String.fromInt (Board.info model.board |> .score)) ]
                ]
            , viewBoard model.board
            ]
        ]


viewBoard : Board.Board -> HM
viewBoard board =
    div
        [ class "pa3 code f2 debug" ]
        [ Html.Keyed.node "div"
            [ style "width" "400px"
            , style "height" "400px"
            ]
            (viewBoardCells board)
        ]


viewBoardCells : Board.Board -> List ( String, HM )
viewBoardCells board =
    let
        { entries, mergedEntries, newIds, newMergedIds, removedIds } =
            Board.info board

        idToAnim : IncId -> TileAnim
        idToAnim id =
            if List.member id newIds then
                Generated

            else if List.member id newMergedIds then
                Merged

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
                    , renderTile pos cell.num Existing
                    )
                )
                mergedEntries

        removedCellViewList : List ( IncId, HM )
        removedCellViewList =
            List.map (\id -> ( id, text "" )) removedIds
    in
    (cellViewList ++ mergedCellViewList ++ removedCellViewList)
        |> List.sortBy (first >> IncId.toInt)
        |> List.map (mapFirst IncId.toString)



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
