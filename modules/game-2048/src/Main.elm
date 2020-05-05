module Main exposing (main)

import Basics.Extra exposing (flip)
import Browser exposing (Document)
import Browser.Events
import Dict exposing (Dict)
import Grid
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as D
import List.Extra
import Random



-- Cons


type alias Cons a =
    ( a, List a )


consFromList : List a -> Maybe (Cons a)
consFromList list =
    case list of
        [] ->
            Nothing

        h :: t ->
            Just ( h, t )


consToList : Cons a -> List a
consToList ( h, t ) =
    h :: t



-- NumList


type alias NumList =
    List Int


type SlideMsg
    = SlideUp
    | SlideDown
    | SlideLeft
    | SlideRight


compactLeft : NumList -> NumList
compactLeft =
    List.reverse >> compactRight >> List.reverse


compactRight : NumList -> NumList
compactRight =
    let
        func v ( maybeUnprocessed, acc ) =
            case maybeUnprocessed of
                Nothing ->
                    ( Just v, acc )

                Just unprocessed ->
                    if unprocessed == v then
                        ( Nothing, unprocessed + v :: acc )

                    else
                        ( Just v, unprocessed :: acc )

        unprocessedTupleToList ( maybeUnprocessed, acc ) =
            case maybeUnprocessed of
                Just head ->
                    head :: acc

                Nothing ->
                    acc
    in
    List.filter (\v -> v /= 0)
        >> List.foldr func ( Nothing, [] )
        >> unprocessedTupleToList
        >> numListPadLeft


numListPadLeft : NumList -> NumList
numListPadLeft l =
    List.repeat (4 - List.length l) 0 ++ l



-- NumGrid


type alias NumGrid =
    Grid.Grid Int


type alias NumPosDict =
    Grid.PosDict Int


type alias NumEntry =
    Grid.Entry Int


numGridSlideAndFillGenerator : SlideMsg -> NumGrid -> Random.Generator (Maybe ( Int, Grid.Pos, NumGrid ))
numGridSlideAndFillGenerator message oldGrid =
    let
        ( score, newGrid ) =
            numGridSlide message oldGrid
    in
    if newGrid /= oldGrid then
        numGridFillRandomEmptyPos newGrid
            |> Random.map (Maybe.map (\( p, g ) -> ( score, p, g )))

    else
        Random.constant Nothing


numGridSlide : SlideMsg -> NumGrid -> ( Int, NumGrid )
numGridSlide message =
    case message of
        SlideUp ->
            Grid.mapColumnLists compactLeft >> Tuple.pair 0

        SlideDown ->
            Grid.mapColumnLists compactRight >> Tuple.pair 0

        SlideLeft ->
            Grid.reverseRows >> numGridCompactRight >> Tuple.mapSecond Grid.reverseRows

        SlideRight ->
            numGridCompactRight


numGridCompactRight : NumGrid -> ( Int, NumGrid )
numGridCompactRight grid =
    let
        ( score, updatedEntries ) =
            List.map compactNumEntriesRight (Grid.toRowEntries grid)
                |> List.foldl (\( a, b ) ( accA, accB ) -> ( a + accA, b ++ accB )) ( 0, [] )
    in
    ( score, Grid.replaceFromEntries updatedEntries grid )


numGridCompactDown : NumGrid -> ( Int, NumGrid )
numGridCompactDown grid =
    let
        ( score, updatedEntries ) =
            List.map compactNumEntriesRight (Grid.toColumnEntries grid)
                |> List.foldl (\( a, b ) ( accA, accB ) -> ( a + accA, b ++ accB )) ( 0, [] )
    in
    ( score, Grid.replaceFromEntries updatedEntries grid )


compactNumEntriesRight : List NumEntry -> ( Int, List NumEntry )
compactNumEntriesRight entries =
    let
        positions =
            List.map Tuple.first entries

        numValues =
            List.map Tuple.second entries
    in
    compactRight2 numValues
        |> Tuple.mapSecond (List.Extra.zip positions)


compactRight2 : NumList -> ( Int, NumList )
compactRight2 =
    let
        func v ( score, ( maybeUnprocessed, processed ) ) =
            case maybeUnprocessed of
                Nothing ->
                    ( score, ( Just v, processed ) )

                Just unprocessed ->
                    if unprocessed == v then
                        ( score + unprocessed + v, ( Nothing, unprocessed + v :: processed ) )

                    else
                        ( score, ( Just v, unprocessed :: processed ) )

        unprocessedTupleToList ( maybeUnprocessed, acc ) =
            case maybeUnprocessed of
                Just head ->
                    head :: acc

                Nothing ->
                    acc
    in
    List.filter (\v -> v /= 0)
        >> List.foldr func ( 0, ( Nothing, [] ) )
        >> Tuple.mapSecond
            (unprocessedTupleToList
                >> numListPadLeft
            )


numGridFillRandomEmptyPos : NumGrid -> Random.Generator (Maybe ( Grid.Pos, NumGrid ))
numGridFillRandomEmptyPos grid =
    case numGridEmptyPositionsCons grid of
        Just posCons ->
            numEntryGenerator posCons
                |> Random.map
                    (\( pos, num ) ->
                        Grid.set pos num grid
                            |> Maybe.map (Tuple.pair pos)
                    )

        Nothing ->
            Random.constant Nothing


numGridEmptyPositionsCons : NumGrid -> Maybe (Cons Grid.Pos)
numGridEmptyPositionsCons grid =
    Grid.toDict grid
        |> Dict.filter (\_ v -> v == 0)
        |> Dict.keys
        |> consFromList


numEntryGenerator : Cons Grid.Pos -> Random.Generator NumEntry
numEntryGenerator ( pos, posList ) =
    Random.pair
        (Random.uniform pos posList)
        (Random.uniform 2 [ 4 ])



-- 2048 Board


type alias Board =
    { seed : Random.Seed
    , grid : NumGrid
    , score : Int
    , lastGen : Maybe Grid.Pos
    }


initBoard : Random.Seed -> Grid.Lists Int -> Board
initBoard seed lists =
    { seed = seed
    , grid = Grid.fromRowLists { width = 4, height = 4 } 0 lists
    , score = 0
    , lastGen = Nothing
    }


updateBoard : SlideMsg -> Board -> Board
updateBoard message board =
    let
        ( maybeScorePosGrid, nextSeed ) =
            Random.step (numGridSlideAndFillGenerator message board.grid) board.seed
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
            Grid.toLists board.grid
    in
    div [ class "measure center" ]
        [ div (class "inline-flex flex-column f4" :: borderStyles)
            (List.indexedMap (viewRow board) rows)
        ]


viewRow : Board -> Int -> NumList -> HM
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



-- GameStatus


type GameStatus
    = Over OverStatus
    | PlayerTurn


type OverStatus
    = Lost
    | Won WonStatus


type WonStatus
    = WonPromptContinue
    | WonContinuePlaying



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
                            Just SlideUp

                        "ArrowDown" ->
                            Just SlideDown

                        "ArrowLeft" ->
                            Just SlideLeft

                        "ArrowRight" ->
                            Just SlideRight

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
