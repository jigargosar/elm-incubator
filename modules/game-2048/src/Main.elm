module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Dict exposing (Dict)
import Grid
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Html.Keyed
import Json.Decode as D
import NumGrid
import Process
import Random
import Task
import UndoList exposing (UndoList)



-- Tile


type TileId
    = TileId Int


tileIdToInt : TileId -> Int
tileIdToInt (TileId i) =
    i


tileIdToString : TileId -> String
tileIdToString =
    tileIdToInt >> String.fromInt


type TileKind
    = MergedTile
    | SimpleTile


type alias Tile =
    { id : TileId
    , num : Int
    , pos : Grid.Pos
    , kind : TileKind
    }


type alias TileCollection =
    List Tile


initTileCollection : TileCollection
initTileCollection =
    [ Tile (TileId 0) 2 ( 0, 0 ) SimpleTile
    , Tile (TileId 1) 2 ( 1, 0 ) SimpleTile
    ]


slideTileCollection : TileCollection -> TileCollection
slideTileCollection _ =
    [ Tile (TileId 0) 2 ( 3, 0 ) SimpleTile
    , Tile (TileId 1) 2 ( 3, 0 ) SimpleTile
    , Tile (TileId 2) 4 ( 3, 0 ) MergedTile
    ]


viewTileCollection : TileCollection -> HM
viewTileCollection tc =
    tc
        |> List.map viewKeyedTile
        |> Html.Keyed.node "div"
            [ style "width" "200px"
            , style "height" "200px"
            ]


viewKeyedTile : Tile -> ( String, HM )
viewKeyedTile tile =
    ( tile.id |> tileIdToString, viewTile tile )


tileTranslate : Tile -> String
tileTranslate tile =
    let
        ( x, y ) =
            tile.pos
    in
    [ "translate", "(", String.fromInt (x * 50), "px", ",", String.fromInt (y * 50), "px", ")" ]
        |> String.join ""


styleTransforms =
    String.join " "
        >> style "transform"


viewTile : Tile -> HM
viewTile tile =
    div
        [ style "width" "50px"
        , style "height" "50px"
        , styleTransforms [ tileTranslate tile ]
        , class "absolute"
        , style "transition" "transform 1s , opacity 1s"
        ]
        [ div
            [ style "width" "50px"
            , style "height" "50px"
            , style "background-color" "rgba(255, 255, 255, .9)"
            , case tile.kind of
                SimpleTile ->
                    class ""

                MergedTile ->
                    class "animated zoomIn"
            ]
            [ text (String.fromInt tile.num) ]
        ]



-- 2048 Board


type alias Board =
    { grid : NumGrid.Model
    , score : Int
    , lastGen : Maybe Grid.Pos
    , seed : Random.Seed
    }


initBoard : Random.Seed -> Grid.Lists Int -> Board
initBoard initialSeed lists =
    { grid = NumGrid.fromRowLists lists
    , score = 0
    , lastGen = Nothing
    , seed = initialSeed
    }


slideBoard : NumGrid.SlideMsg -> Board -> Maybe Board
slideBoard message board =
    NumGrid.update message board.grid
        |> Maybe.map (slideBoardHelp board)


slideBoardHelp : Board -> Random.Generator ( Int, Grid.Pos, NumGrid.Model ) -> Board
slideBoardHelp board generator =
    let
        ( ( score, pos, numGrid ), seed ) =
            Random.step generator board.seed
    in
    { board
        | grid = numGrid
        , lastGen = Just pos
        , score = board.score + score
        , seed = seed
    }


hasWon : Board -> Bool
hasWon board =
    NumGrid.hasWon board.grid


hasLost : Board -> Bool
hasLost board =
    NumGrid.hasLost board.grid


viewBoardHeader : Board -> HM
viewBoardHeader board =
    div [ class "measure center" ]
        [ div [ class "f3 pv3" ] [ text "2048 grid" ]
        , div [ class "pv1 flex items-center" ]
            [ div [ class "pa2" ] [ text "Score: ", text (String.fromInt board.score) ]
            , button [ onClick UndoClicked ] [ text "Undo" ]
            , button [ onClick NewClicked ] [ text "New" ]
            ]
        ]


viewBoard : Board -> HM
viewBoard board =
    let
        rows =
            board.grid
                |> NumGrid.toGrid
                |> Grid.toLists
    in
    div [ class "measure center" ]
        [ div (class "inline-flex flex-column f4" :: borderStyles)
            (List.indexedMap (viewRow board) rows)
        ]


viewRow : Board -> Int -> List Int -> HM
viewRow board ri row =
    div [ class "flex" ] (List.indexedMap (viewCell board ri) row)


viewCell : Board -> Int -> Int -> Int -> HM
viewCell board ri ci num =
    let
        cellContainerStyle =
            class "w3 h2 flex items-center justify-center"
    in
    if Just ( ci, ri ) == board.lastGen then
        div (cellContainerStyle :: highlightedBorderStyles) [ viewNumString num ]

    else
        div (cellContainerStyle :: borderStyles) [ viewNumString num ]


borderStyles =
    [ class "ba b--silver" ]


highlightedBorderStyles =
    [ class "ba b--red" ]


viewNumString num =
    text
        (case num of
            0 ->
                ""

            _ ->
                String.fromInt num
                    --|> always "2048"
                    |> identity
        )



-- Model


type alias UndoBoard =
    UndoList Board


type alias Model =
    { undoBoard : UndoBoard
    , state : State
    , tc : TileCollection
    }


type State
    = Turn Bool
    | Won
    | Lost


type alias Flags =
    ()


initialUndoBoard =
    initBoard (Random.initialSeed 0)
        ([ [ 0, 2, 2, 0 ]
         , [ 2, 4, 2, 2 ]
         , [ 2, 2, 4, 2 ]
         , [ 0, 2, 2, 0 ]
         ]
            |> always [ [ 2 ] ]
        )
        |> UndoList.fresh


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { undoBoard = initialUndoBoard
      , state = Turn False
      , tc = initTileCollection
      }
    , delayN 2000 SlideTC
    )


delayN n msg =
    Process.sleep n
        |> Task.perform (always msg)



-- Update


type Msg
    = NoOp
    | OnKeyDown String
    | UndoClicked
    | NewClicked
    | ContinueClicked
    | SlideTC


type UpdateBoardMsg
    = SlideMsg NumGrid.SlideMsg
    | UndoMove


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        OnKeyDown string ->
            let
                maybeUpdateBoardMsg =
                    case string of
                        "ArrowUp" ->
                            Just (SlideMsg NumGrid.SlideUp)

                        "ArrowDown" ->
                            Just (SlideMsg NumGrid.SlideDown)

                        "ArrowLeft" ->
                            Just (SlideMsg NumGrid.SlideLeft)

                        "ArrowRight" ->
                            Just (SlideMsg NumGrid.SlideRight)

                        "u" ->
                            Just UndoMove

                        _ ->
                            Nothing
            in
            case maybeUpdateBoardMsg of
                Just updateBoardMsg ->
                    case updateBoardMsg of
                        SlideMsg slideMsg ->
                            ( updateUndoListBoard slideMsg model, Cmd.none )

                        UndoMove ->
                            ( undoMove model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UndoClicked ->
            ( undoMove model, Cmd.none )

        NewClicked ->
            ( { model | undoBoard = initialUndoBoard, state = Turn False }, Cmd.none )

        ContinueClicked ->
            case model.state of
                Won ->
                    ( { model | state = Turn True }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SlideTC ->
            ( { model | tc = slideTileCollection model.tc }, Cmd.none )


undoMove : Model -> Model
undoMove model =
    case model.state of
        Turn isContinuingAfterVictory ->
            let
                newUndoBoard =
                    UndoList.undo model.undoBoard
            in
            { model
                | undoBoard = newUndoBoard
                , state =
                    if isContinuingAfterVictory && not (UndoList.view hasWon newUndoBoard) then
                        Turn False

                    else
                        model.state
            }

        _ ->
            model


updateUndoListBoard : NumGrid.SlideMsg -> Model -> Model
updateUndoListBoard slideMsg model =
    case model.state of
        Turn isContinuingAfterVictory ->
            case UndoList.view (slideBoard slideMsg) model.undoBoard of
                Just newBoard ->
                    { model
                        | undoBoard = UndoList.new newBoard model.undoBoard
                        , state =
                            if not isContinuingAfterVictory && hasWon newBoard then
                                Won

                            else if hasLost newBoard then
                                Lost

                            else
                                model.state
                    }

                Nothing ->
                    model

        _ ->
            model


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown
            (D.field "key" D.string |> D.map OnKeyDown)
        ]



-- View


type alias DM =
    Document Msg


view : Model -> DM
view model =
    Document "2048"
        [ div [ class "relative" ]
            [ div []
                [ UndoList.view viewBoardHeader model.undoBoard
                , UndoList.view viewBoard model.undoBoard
                ]
            , case model.state of
                Won ->
                    dialogContainer
                        [ div [] [ text "Won" ]
                        , div [ onClick NewClicked ] [ button [] [ text "Try Again" ] ]
                        , div [ onClick ContinueClicked ] [ button [] [ text "Continue Playing" ] ]
                        ]

                Lost ->
                    dialogContainer
                        [ div [] [ text "Lost" ]
                        , div [ onClick NewClicked ] [ button [] [ text "Try Again" ] ]
                        ]

                Turn _ ->
                    text ""
            ]
        , div [ class "measure center pv4 debug" ]
            [ viewTileCollection model.tc
            ]
        ]


dialogContainer content =
    div [ class "absolute absolute--fill" ]
        [ div [ class "mt5 measure center pa4 br3 bg-white o-90 shadow-1" ]
            content
        ]


type alias HM =
    Html Msg



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
