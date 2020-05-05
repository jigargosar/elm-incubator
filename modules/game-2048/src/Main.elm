module Main exposing (main)

import Basics.Extra exposing (uncurry)
import Browser exposing (Document)
import Browser.Events
import Dict exposing (Dict)
import Grid
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as D
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



-- NumList


type alias NumList =
    List Int


type SlideMsg
    = SlideUp
    | SlideDown
    | SlideLeft
    | SlideRight


slideGrid : NumGrid -> ( Int, NumGrid )
slideGrid grid =
    let
        _ =
            1

        --    for every position get its right
        -- if current is zero; continue
        -- if current is non-zero;
        --      get first non zero right;
        --          if right doesn't exist ; continue with next row
        --          if current/=right; continue; from right index;
        --          if current == right; set current to zero; set right to double and add to score; continue from next to right;
    in
    ( 0, grid )


type alias CombineAcc =
    {}


type CombineMsg
    = ContinueWithNextPos
    | CombineCurrent
    | CombineEntry NumEntry


accNextPos : CombineAcc -> Maybe CombineAcc
accNextPos acc =
    Debug.todo "impl"


accToReturn : CombineAcc -> ( Int, NumPosDict )
accToReturn acc =
    Debug.todo "impl"


accNextRow : CombineAcc -> Maybe CombineAcc
accNextRow acc =
    Debug.todo "impl"


accGotoPos : Grid.Pos -> CombineAcc -> Maybe CombineAcc
accGotoPos pos acc =
    Debug.todo "impl"


accCurrentNum : CombineAcc -> Int
accCurrentNum acc =
    Debug.todo "impl"


accFindNonZeroEntry : CombineAcc -> Maybe NumEntry
accFindNonZeroEntry acc =
    Debug.todo "impl"


accCombineAndGotoEntry : NumEntry -> CombineAcc -> Maybe CombineAcc
accCombineAndGotoEntry numEntry acc =
    Debug.todo "impl"


combineRightLoop : CombineMsg -> CombineAcc -> ( Int, NumPosDict )
combineRightLoop message acc =
    case message of
        ContinueWithNextPos ->
            case accNextPos acc of
                Just nac ->
                    combineRightLoop CombineCurrent nac

                Nothing ->
                    accToReturn acc

        CombineEntry ne ->
            case accCombineAndGotoEntry ne acc of
                Just nac ->
                    combineRightLoop CombineCurrent nac

                Nothing ->
                    Debug.todo "should never happen"

        CombineCurrent ->
            case accCurrentNum acc of
                0 ->
                    combineRightLoop ContinueWithNextPos acc

                currentVal ->
                    case accFindNonZeroEntry acc of
                        Nothing ->
                            combineRightLoop ContinueWithNextPos acc

                        Just ( rightPos, rightVal ) ->
                            if currentVal == rightVal then
                                combineRightLoop (CombineEntry ( rightPos, rightVal )) acc

                            else
                                combineRightLoop ContinueWithNextPos acc


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


numGridSlideAndFillGenerator : SlideMsg -> NumGrid -> Random.Generator (Maybe ( Grid.Pos, NumGrid ))
numGridSlideAndFillGenerator message oldGrid =
    let
        newGrid =
            numGridSlide message oldGrid
    in
    if newGrid /= oldGrid then
        numGridFillRandomEmptyPos newGrid

    else
        Random.constant Nothing


numGridSlide : SlideMsg -> NumGrid -> NumGrid
numGridSlide message =
    case message of
        SlideUp ->
            Grid.mapColumnLists compactLeft

        SlideDown ->
            Grid.mapColumnLists compactRight

        SlideLeft ->
            Grid.mapRowLists compactLeft

        SlideRight ->
            Grid.mapRowLists compactRight


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
        ( maybePosGrid, nextSeed ) =
            Random.step (numGridSlideAndFillGenerator message board.grid) board.seed
    in
    case maybePosGrid of
        Just ( pos, grid ) ->
            { board
                | grid = grid
                , lastGen = Just pos
                , seed = nextSeed
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
