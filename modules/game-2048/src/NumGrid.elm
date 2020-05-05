module NumGrid exposing (NumGrid, SlideMsg(..), numGridSlideAndFillGenerator)

import Dict
import Grid
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


type SlideMsg
    = SlideUp
    | SlideDown
    | SlideLeft
    | SlideRight


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
