module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Dict exposing (Dict)
import Grid
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as D
import List.Extra
import NumGrid
import Random



-- 2048 Board


type alias Board =
    { seed : Random.Seed
    , grid : NumGrid.Model
    , score : Int
    , lastGen : Maybe Grid.Pos
    }


initBoard : Random.Seed -> Grid.Lists Int -> Board
initBoard seed lists =
    { seed = seed
    , grid = NumGrid.fromRowLists lists
    , score = 0
    , lastGen = Nothing
    }


updateBoard : NumGrid.Msg -> Board -> Board
updateBoard message board =
    let
        ( maybeScorePosGrid, nextSeed ) =
            Random.step (NumGrid.update message board.grid) board.seed
    in
    case maybeScorePosGrid of
        Just ( score, pos, grid ) ->
            { board
                | grid = grid
                , lastGen = Just pos
                , seed = nextSeed
                , score = board.score + score
            }

        Nothing ->
            board


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
    { board : Board }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { board =
            initBoard (Random.initialSeed 0)
                ([ [ 0, 2, 2, 0 ]
                 , [ 2, 4, 2, 2 ]
                 , [ 2, 2, 4, 2 ]
                 , [ 0, 2, 2, 0 ]
                 ]
                    |> always [ [ 2 ] ]
                )
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | OnKeyDown String


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        OnKeyDown string ->
            let
                maybeSlideMsg =
                    case string of
                        "ArrowUp" ->
                            Just NumGrid.SlideUp

                        "ArrowDown" ->
                            Just NumGrid.SlideDown

                        "ArrowLeft" ->
                            Just NumGrid.SlideLeft

                        "ArrowRight" ->
                            Just NumGrid.SlideRight

                        _ ->
                            Nothing
            in
            case maybeSlideMsg of
                Just slideMsg ->
                    ( { model | board = updateBoard slideMsg model.board }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


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
        , viewScore model.board
        , viewBoard model.board
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
