module Main exposing (main)

import Basics.Extra exposing (flip)
import Browser exposing (Document)
import Browser.Events
import Grid
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as D
import MaybeGenerator exposing (MaybeGenerator)
import NumGrid
import Random
import Seeded exposing (Seeded)
import UndoList exposing (UndoList)



-- 2048 Board


type alias Board =
    { grid : NumGrid.Model
    , score : Int
    , lastGen : Maybe Grid.Pos
    }


initBoard : Grid.Lists Int -> Board
initBoard lists =
    { grid = NumGrid.fromRowLists lists
    , score = 0
    , lastGen = Nothing
    }


slideBoard : NumGrid.SlideMsg -> Board -> MaybeGenerator Board
slideBoard message board =
    NumGrid.update message board.grid
        |> MaybeGenerator.map
            (\( score, pos, numGrid ) ->
                { board
                    | grid = numGrid
                    , lastGen = Just pos
                    , score = board.score + score
                }
            )


viewScore : Board -> HM
viewScore board =
    div [ class "measure center" ]
        [ div [ class " pa2" ]
            [ text "Score: "
            , text (String.fromInt board.score)
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


type alias Model =
    { board : UndoList (Seeded Board)
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    let
        initialSeed =
            Random.initialSeed 0
    in
    ( { board =
            initBoard
                ([ [ 0, 2, 2, 0 ]
                 , [ 2, 4, 2, 2 ]
                 , [ 2, 2, 4, 2 ]
                 , [ 0, 2, 2, 0 ]
                 ]
                    |> always [ [ 2 ] ]
                )
                |> Seeded.init initialSeed
                |> UndoList.fresh
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | OnKeyDown String
    | Undo


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
                            ( updateAndGenerateUndoListSeededBoard slideMsg model, Cmd.none )

                        UndoMove ->
                            ( undoMove model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Undo ->
            ( undoMove model, Cmd.none )


undoMove : { a | board : UndoList state } -> { a | board : UndoList state }
undoMove model =
    { model | board = UndoList.undo model.board }


updateAndGenerateUndoListSeededBoard : NumGrid.SlideMsg -> Model -> Model
updateAndGenerateUndoListSeededBoard message model =
    model.board
        |> UndoList.view identity
        |> Seeded.maybeStep (slideBoard message)
        |> Maybe.map (flip UndoList.new model.board >> flip setBoard model)
        |> Maybe.withDefault model


setBoard board model =
    { model | board = board }


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
        [ div [ class "f3 pa3" ] [ text "2048 grid" ]
        , button [ onClick Undo ] [ text "Undo" ]
        , UndoList.view (\board -> viewScore (Seeded.get board)) model.board
        , UndoList.view (\board -> viewBoard (Seeded.get board)) model.board
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
