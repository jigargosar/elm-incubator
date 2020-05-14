module Main exposing (main)

import Board exposing (Board, Msg(..))
import Browser exposing (Document)
import Browser.Events
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Html.Keyed
import IncId exposing (IncId)
import IntPos exposing (IntPos)
import IntSize
import Json.Decode as JD exposing (Decoder)
import Random
import Tuple exposing (..)



-- Model


type alias Model =
    { status : Status
    , board : Board
    }


type Status
    = Turn
    | NoMoves
    | Won


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    let
        seed =
            Random.initialSeed 0
    in
    ( Model Turn (Board.init seed)
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
            case model.status of
                Turn ->
                    case
                        updateBoardFromKey key model.board
                            |> Maybe.map (boardToModel (Board.hasWon model.board))
                    of
                        Just newModel ->
                            ( newModel, Cmd.none )

                        Nothing ->
                            if Board.noMovesLeft model.board then
                                ( Model NoMoves model.board, Cmd.none )

                            else
                                ( model, Cmd.none )

                NoMoves ->
                    ( model, Cmd.none )

                Won ->
                    ( model, Cmd.none )

        ContinueClicked ->
            case model.status of
                Turn ->
                    ( model, Cmd.none )

                NoMoves ->
                    ( model, Cmd.none )

                Won ->
                    ( Model Turn model.board, Cmd.none )

        NewClicked ->
            ( Model Turn (Board.reInit model.board), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Browser.Events.onKeyDown onKeyDown ]


onKeyDown : Decoder Msg
onKeyDown =
    JD.field "key" JD.string
        |> JD.map OnKeyDown


updateBoardFromKey : String -> Board -> Maybe Board
updateBoardFromKey key board =
    case key of
        "ArrowLeft" ->
            Board.update SlideLeft board

        "ArrowRight" ->
            Board.update SlideRight board

        "ArrowUp" ->
            Board.update SlideUp board

        "ArrowDown" ->
            Board.update SlideDown board

        _ ->
            Nothing


boardToModel : Bool -> Board -> Model
boardToModel alreadyWon board =
    if alreadyWon then
        Model Turn board

    else if Board.hasWon board then
        Model Won board

    else
        Model Turn board



-- View


type alias DM =
    Document Msg


type alias HM =
    Html Msg


view : Model -> DM
view model =
    Document "2048 Animated"
        [ div [ class "measure center" ]
            [ viewHeader model.board
            , viewBoard model
            ]
        ]


viewHeader : Board -> HM
viewHeader board =
    div [ class "pa2 flex items-center" ]
        [ div [ class "pa2" ] [ text ("Score: " ++ String.fromInt (Board.info board |> .score)) ]
        , div [ class "pa2" ] [ button [ onClick NewClicked ] [ text "New" ] ]
        ]


viewBoard : Model -> HM
viewBoard model =
    div
        [ class "code f2 relative center mv4 overflow-hidden"
        , style "width" (String.fromInt gridWidth ++ "px")
        , style "height" (String.fromInt gridWidth ++ "px")
        , style "background-color" "hsl(29, 17%, 68%)"
        , style "border-radius" "6px"
        ]
        (viewGridBackgroundCells
            :: viewGridCells model.board
            :: (case model.status of
                    Turn ->
                        []

                    NoMoves ->
                        [ div
                            [ class "absolute top-0 bg-white o-90 pa4 w-100 h-100 flex"
                            , class "animate__animated  animate__fadeIn animate__delay-4s"
                            ]
                            [ text "Game Over : No Moves Left" ]
                        ]

                    Won ->
                        [ div
                            [ class "absolute top-0 bg-white o-90 pa4 w-100 h-100 flex"
                            , class "animate__animated  animate__fadeIn animate__delay-4s"
                            ]
                            [ div [ class "pa2" ] [ text "You Won!" ]
                            , div [ class "pa2" ] [ button [ onClick ContinueClicked ] [ text "Continue?" ] ]
                            ]
                        ]
               )
        )


viewGridBackgroundCells : HM
viewGridBackgroundCells =
    div [ class "absolute w-100 h-100" ] (List.map viewCellBackgroundTile (IntSize.positions Board.size))


viewCellBackgroundTile : IntPos -> HM
viewCellBackgroundTile pos =
    div
        [ class "absolute"
        , style "width" (String.fromInt cellWidth ++ "px")
        , style "height" (String.fromInt cellWidth ++ "px")
        , style "transform" (renderTileTransform pos)
        , style "border-radius" "3px"
        , style "background-color" "hsla(30, 37%, 89%, 0.35)"
        ]
        []


viewGridCells : Board -> HM
viewGridCells board =
    Html.Keyed.node "div" [ class "absolute w-100 h-100" ] (viewKeyedCells board)


viewKeyedCells : Board -> List ( String, HM )
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
        [ class "absolute"
        , style "width" (String.fromInt cellWidth ++ "px")
        , style "height" (String.fromInt cellWidth ++ "px")
        , class "flex justify-center items-center"
        , style "transform" (renderTileTransform pos)
        , style "transition" "transform 150ms"
        ]
        [ div
            [ style "background-color" "rgba(255,255,255,0.9)"
            , style "border-radius" "3px"
            , class "w-100 h-100 flex justify-center items-center"
            , style "font-size" (String.fromInt (cellWidth // 2) ++ "px")
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


toBackgroundColor : Int -> String
toBackgroundColor n =
    case n of
        2 ->
            ""

        4 ->
            ""

        8 ->
            ""

        16 ->
            ""

        32 ->
            ""

        64 ->
            ""

        128 ->
            ""

        256 ->
            ""

        512 ->
            ""

        1024 ->
            ""

        2048 ->
            ""

        4096 ->
            ""

        _ ->
            ""


cellWidth =
    100


cellSpacing =
    15


gridWidth =
    4 * cellWidth + 5 * cellSpacing


renderTileTransform pos =
    let
        postPartToPx n =
            String.fromInt ((n * cellWidth) + ((n + 1) * cellSpacing)) ++ "px"

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
