module Main exposing (main)

import Basics.Extra exposing (flip)
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
    { board : UndoList Board
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
            initBoard initialSeed
                ([ [ 0, 2, 2, 0 ]
                 , [ 2, 4, 2, 2 ]
                 , [ 2, 2, 4, 2 ]
                 , [ 0, 2, 2, 0 ]
                 ]
                    |> always [ [ 2 ] ]
                )
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
        |> slideBoard message
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
        , UndoList.view viewScore model.board
        , UndoList.view viewBoard model.board
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
