module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Grid
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as D
import NumGrid
import Random
import UndoList exposing (UndoList)



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
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | OnKeyDown String
    | UndoClicked
    | NewClicked
    | ContinueClicked


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
            ( { undoBoard = initialUndoBoard, state = Turn False }, Cmd.none )

        ContinueClicked ->
            case model.state of
                Won ->
                    ( { model | state = Turn True }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


undoMove : Model -> Model
undoMove model =
    case model.state of
        Turn _ ->
            { model | undoBoard = UndoList.undo model.undoBoard }

        _ ->
            model


updateUndoListBoard : NumGrid.SlideMsg -> Model -> Model
updateUndoListBoard message model =
    case model.state of
        Turn isContinuingAfterVictory ->
            model.undoBoard
                |> UndoList.view
                    (\board ->
                        case slideBoard message board of
                            Just newBoard ->
                                { model
                                    | undoBoard = UndoList.new newBoard model.undoBoard
                                    , state =
                                        if not isContinuingAfterVictory && hasWon board then
                                            Won

                                        else if hasLost board then
                                            Lost

                                        else
                                            model.state
                                }

                            Nothing ->
                                model
                    )

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
                        , div [ onClick NewClicked ] [ button [] [ text "Continue" ] ]
                        ]

                Lost ->
                    dialogContainer
                        [ div [] [ text "Lost" ]
                        , div [ onClick NewClicked ] [ button [] [ text "Continue" ] ]
                        ]

                Turn _ ->
                    text ""
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
