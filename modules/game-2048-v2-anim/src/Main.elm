module Main exposing (main)

import Board exposing (Msg(..))
import Browser exposing (Document)
import Browser.Events
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Html.Keyed
import IncId exposing (IncId)
import IntPos exposing (IntPos)
import Json.Decode as JD exposing (Decoder)
import Random
import Tuple exposing (..)



-- Model


type Model
    = Turn Board.Board
    | NoMoves Board.Board
    | Won Board.Board


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    let
        seed =
            Random.initialSeed 0
    in
    ( Turn (Board.init seed)
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | OnKeyDown String
    | NewClicked
    | ContinueClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        OnKeyDown key ->
            case model of
                Turn board ->
                    case updateBoardFromKey key board of
                        Just newBoard ->
                            let
                                alreadyWon =
                                    Board.hasWon board

                                justWon =
                                    Board.hasWon newBoard
                            in
                            if alreadyWon then
                                ( Turn newBoard, Cmd.none )

                            else if justWon then
                                ( Won board, Cmd.none )

                            else
                                ( Turn board, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                NoMoves _ ->
                    ( model, Cmd.none )

                Won _ ->
                    ( model, Cmd.none )

        ContinueClicked ->
            case model of
                Turn _ ->
                    ( model, Cmd.none )

                NoMoves _ ->
                    ( model, Cmd.none )

                Won board ->
                    ( Turn board, Cmd.none )

        NewClicked ->
            case model of
                Turn board ->
                    ( Turn (Board.reInit board), Cmd.none )

                NoMoves board ->
                    ( Turn (Board.reInit board), Cmd.none )

                Won board ->
                    ( Turn (Board.reInit board), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Browser.Events.onKeyDown onKeyDown ]


onKeyDown : Decoder Msg
onKeyDown =
    JD.field "key" JD.string
        |> JD.map OnKeyDown


updateBoardFromKey : String -> Board.Board -> Maybe Board.Board
updateBoardFromKey key board =
    case key of
        "ArrowLeft" ->
            Just (Board.update SlideLeft board)

        "ArrowRight" ->
            Just (Board.update SlideRight board)

        "ArrowUp" ->
            Just (Board.update SlideUp board)

        "ArrowDown" ->
            Just (Board.update SlideDown board)

        _ ->
            Nothing



-- View


type alias DM =
    Document Msg


type alias HM =
    Html Msg


view : Model -> DM
view model =
    Document "2048 Animated"
        [ div [ class "measure center" ]
            (case model of
                Turn board ->
                    [ viewHeader board
                    , viewBoard board
                    ]

                NoMoves board ->
                    [ viewHeader board
                    , div [ class "pa2" ] [ text "No Moves Left" ]
                    , viewBoard board
                    ]

                Won board ->
                    [ viewHeader board
                    , div []
                        [ div [ class "pa2" ] [ text "You Won!" ]
                        , div [ class "pa2" ] [ button [ onClick ContinueClicked ] [ text "Continue?" ] ]
                        ]
                    , viewBoard board
                    ]
            )
        ]


viewHeader : Board.Board -> HM
viewHeader board =
    div [ class "pa2 flex items-center" ]
        [ div [ class "pa2" ] [ text ("Score: " ++ String.fromInt (Board.info board |> .score)) ]
        , div [ class "pa2" ] [ button [ onClick NewClicked ] [ text "New" ] ]
        ]


viewBoard : Board.Board -> HM
viewBoard board =
    div
        [ class "pa3 code f2 debug" ]
        [ Html.Keyed.node "div"
            [ style "width" "400px"
            , style "height" "400px"
            ]
            (viewKeyedCells board)
        ]


viewKeyedCells : Board.Board -> List ( String, HM )
viewKeyedCells board =
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
                        , viewTile pos cell.num (idToAnim cell.id)
                        )
                    )

        mergedCellViewList : List ( IncId, HM )
        mergedCellViewList =
            List.map
                (\( pos, cell ) ->
                    ( cell.id
                    , viewTile pos cell.num Existing
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


type TileAnim
    = Generated
    | Merged
    | Existing


viewTile : IntPos -> Int -> TileAnim -> HM
viewTile pos num anim =
    div
        [ style "width" "100px"
        , style "height" "100px"
        , class "absolute flex justify-center items-center"
        , style "transform" (renderTileTransform pos)
        , style "transition" "transform 150ms"
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
