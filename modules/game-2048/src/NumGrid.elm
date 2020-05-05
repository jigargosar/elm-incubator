module NumGrid exposing (Model, Msg(..), fromRowLists, toGrid, update)

import Dict
import Grid
import List.Extra
import Random



-- PUBLIC


type Model
    = Model NumGrid


fromRowLists : List (List Int) -> Model
fromRowLists =
    Grid.fromRowLists { width = 4, height = 4 } 0
        >> Model


type Msg
    = SlideUp
    | SlideDown
    | SlideLeft
    | SlideRight


update : Msg -> Model -> Maybe (Random.Generator ( Int, Grid.Pos, Model ))
update message (Model grid) =
    numGridUpdate message grid
        |> Maybe.map
            (Random.map
                (\( score, pos, nextNumGrid ) ->
                    ( score, pos, Model nextNumGrid )
                )
            )


toGrid : Model -> Grid.Grid Int
toGrid (Model grid) =
    grid



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


numListCompactRight : NumList -> ( Int, NumList )
numListCompactRight =
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
            (unprocessedTupleToList >> numListPadLeft)


numListPadLeft : NumList -> NumList
numListPadLeft l =
    List.repeat (4 - List.length l) 0 ++ l



-- NumGrid


type alias NumGrid =
    Grid.Grid Int


type alias NumEntry =
    Grid.Entry Int


numGridUpdate : Msg -> NumGrid -> Maybe (Random.Generator ( Int, Grid.Pos, NumGrid ))
numGridUpdate message oldGrid =
    let
        ( score, newGrid ) =
            numGridSlide message oldGrid
    in
    if newGrid /= oldGrid then
        numGridFillRandomEmptyPos newGrid
            |> Maybe.map
                (Random.map
                    (\( p, g ) -> ( score, p, g ))
                )

    else
        Nothing


numGridSlide : Msg -> NumGrid -> ( Int, NumGrid )
numGridSlide message =
    case message of
        SlideUp ->
            numGridCompactUp

        SlideDown ->
            numGridCompactDown

        SlideLeft ->
            numGridCompactLeft

        SlideRight ->
            numGridCompactRight


numGridCompactRight : NumGrid -> ( Int, NumGrid )
numGridCompactRight =
    numGridCompactHelp Grid.toRowEntries


numGridCompactLeft : NumGrid -> ( Int, NumGrid )
numGridCompactLeft =
    Grid.reverseRows >> numGridCompactRight >> Tuple.mapSecond Grid.reverseRows


numGridCompactDown : NumGrid -> ( Int, NumGrid )
numGridCompactDown =
    numGridCompactHelp Grid.toColumnEntries


numGridCompactUp : NumGrid -> ( Int, NumGrid )
numGridCompactUp =
    Grid.reverseColumns >> numGridCompactHelp Grid.toColumnEntries >> Tuple.mapSecond Grid.reverseColumns


numGridCompactHelp : (NumGrid -> List (List NumEntry)) -> NumGrid -> ( Int, NumGrid )
numGridCompactHelp toNumEntryLists grid =
    let
        ( score, updatedEntries ) =
            List.map numEntriesCompactRight (toNumEntryLists grid)
                |> List.foldl (\( a, b ) ( accA, accB ) -> ( a + accA, b ++ accB )) ( 0, [] )
    in
    ( score, Grid.replaceEntries updatedEntries grid )


numEntriesCompactRight : List NumEntry -> ( Int, List NumEntry )
numEntriesCompactRight entries =
    let
        positions =
            List.map Tuple.first entries

        numValues =
            List.map Tuple.second entries
    in
    numListCompactRight numValues
        |> Tuple.mapSecond (List.Extra.zip positions)


numGridFillRandomEmptyPos : NumGrid -> Maybe (Random.Generator ( Grid.Pos, NumGrid ))
numGridFillRandomEmptyPos grid =
    let
        func ( pos, num ) =
            case Grid.set pos num grid of
                Nothing ->
                    Debug.todo "This should never happen"

                Just filledGrid ->
                    ( pos, filledGrid )
    in
    numGridEmptyPositionsCons grid
        |> Maybe.map (numEntryGenerator >> Random.map func)


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
