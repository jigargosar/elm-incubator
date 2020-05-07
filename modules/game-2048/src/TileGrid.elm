module TileGrid exposing (TileGrid, fromNumRows)

import Basics.Extra exposing (flip, swap, uncurry)
import Dict
import Grid
import List.Extra
import Random


type TileGrid
    = TileGrid CellIdGenerator (Grid.Grid Cell)


size =
    { width = 4, height = 4 }


fromNumRows : List (List Int) -> TileGrid
fromNumRows lists =
    lists
        |> List.Extra.mapAccuml
            (List.Extra.mapAccuml
                (\idGen num ->
                    case num of
                        0 ->
                            ( idGen, EmptyCell )

                        _ ->
                            newCell num InitialCell idGen |> swap
                )
            )
            initCellIdGenerator
        |> Tuple.mapSecond (Grid.fromRowLists size EmptyCell)
        |> uncurry TileGrid


type SlideMsg
    = SlideUp
    | SlideDown
    | SlideLeft
    | SlideRight



-- CellId


type CellId
    = CellId String


type CellIdGenerator
    = CellIdGenerator Int


initCellIdGenerator : CellIdGenerator
initCellIdGenerator =
    CellIdGenerator 1


newCellId : CellIdGenerator -> ( CellId, CellIdGenerator )
newCellId (CellIdGenerator nextId) =
    ( CellId (String.fromInt nextId), CellIdGenerator (nextId + 1) )



-- Cell


type Cell
    = NumCell CellId Int CellKind
    | EmptyCell


type CellKind
    = InitialCell
    | GeneratedCell
    | MergedCell CellId CellId


newCell : Int -> CellKind -> CellIdGenerator -> ( Cell, CellIdGenerator )
newCell num kind =
    newCellId >> Tuple.mapFirst (\id -> NumCell id num kind)



-- CellGrid


type alias CellGrid =
    Grid.Grid Cell


type alias CellEntry =
    Grid.Entry Cell


cellGridUpdate :
    SlideMsg
    -> CellIdGenerator
    -> CellGrid
    -> Maybe (Random.Generator ( ( Int, Grid.Pos, CellIdGenerator ), CellGrid ))
cellGridUpdate message idGen0 oldGrid =
    let
        ( ( score, idGen1 ), newGrid ) =
            cellGridSlide message idGen0 oldGrid
    in
    if newGrid /= oldGrid then
        cellGridFillRandom idGen1 newGrid
            |> Maybe.map (Random.map (\( ( p, idGen ), g ) -> ( ( score, p, idGen ), g )))

    else
        Nothing



-- FILL


cellGridFillRandom : CellIdGenerator -> CellGrid -> Maybe (Random.Generator ( ( Grid.Pos, CellIdGenerator ), CellGrid ))
cellGridFillRandom idGen0 grid =
    let
        func ( pos, num ) =
            let
                ( cell, idGen ) =
                    newCell num GeneratedCell idGen0
            in
            case Grid.set pos cell grid of
                Nothing ->
                    Debug.todo "This should never happen"

                Just filledGrid ->
                    ( ( pos, idGen ), filledGrid )
    in
    cellGridEmptyPositionsCons grid
        |> Maybe.map (cellEntryGenerator >> Random.map func)


cellGridEmptyPositionsCons : CellGrid -> Maybe (Cons Grid.Pos)
cellGridEmptyPositionsCons grid =
    Grid.toDict grid
        |> Dict.filter (\_ v -> v == EmptyCell)
        |> Dict.keys
        |> consFromList


cellEntryGenerator : Cons Grid.Pos -> Random.Generator ( Grid.Pos, Int )
cellEntryGenerator ( pos, posList ) =
    Random.pair
        (Random.uniform pos posList)
        (Random.uniform 2 [ 4 ])



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



-- FILL END


cellGridSlide : SlideMsg -> CellIdGenerator -> CellGrid -> ( ( Int, CellIdGenerator ), CellGrid )
cellGridSlide message =
    case message of
        SlideUp ->
            cellGridCompactUp

        SlideDown ->
            cellGridCompactDown

        SlideLeft ->
            cellGridCompactLeft

        SlideRight ->
            cellGridCompactRight


cellGridCompactRight : CellIdGenerator -> CellGrid -> ( ( Int, CellIdGenerator ), CellGrid )
cellGridCompactRight idGen =
    cellGridCompactHelp idGen Grid.toRowEntries


cellGridCompactLeft : CellIdGenerator -> CellGrid -> ( ( Int, CellIdGenerator ), CellGrid )
cellGridCompactLeft idGen =
    Grid.reverseRows >> cellGridCompactHelp idGen Grid.toRowEntries >> Tuple.mapSecond Grid.reverseRows


cellGridCompactDown : CellIdGenerator -> CellGrid -> ( ( Int, CellIdGenerator ), CellGrid )
cellGridCompactDown idGen =
    cellGridCompactHelp idGen Grid.toColumnEntries


cellGridCompactUp : CellIdGenerator -> CellGrid -> ( ( Int, CellIdGenerator ), CellGrid )
cellGridCompactUp idGen =
    Grid.reverseColumns >> cellGridCompactHelp idGen Grid.toColumnEntries >> Tuple.mapSecond Grid.reverseColumns


cellGridCompactHelp : CellIdGenerator -> (CellGrid -> List (List CellEntry)) -> CellGrid -> ( ( Int, CellIdGenerator ), CellGrid )
cellGridCompactHelp idGen toCellEntryLists grid =
    List.Extra.mapAccuml cellEntriesCompactRight ( 0, idGen ) (toCellEntryLists grid)
        |> Tuple.mapSecond (List.concat >> flip Grid.replaceEntries grid)


cellEntriesCompactRight : ( Int, CellIdGenerator ) -> List CellEntry -> ( ( Int, CellIdGenerator ), List CellEntry )
cellEntriesCompactRight acc entries =
    let
        positions =
            List.map Tuple.first entries

        cellValues =
            List.map Tuple.second entries
    in
    cellListCompactRight acc cellValues
        |> Tuple.mapSecond (List.Extra.zip positions)



-- Cell List


type alias CellList =
    List Cell


type alias CompactAcc =
    ( ( Int, CellIdGenerator ), ( Maybe Cell, CellList ) )


cellListCompactRight : ( Int, CellIdGenerator ) -> CellList -> ( ( Int, CellIdGenerator ), CellList )
cellListCompactRight acc0 =
    let
        func : Cell -> CompactAcc -> CompactAcc
        func cell (( ( score, idGen ), ( maybeUnprocessed, processed ) ) as acc) =
            case ( cell, maybeUnprocessed ) of
                ( EmptyCell, _ ) ->
                    acc

                ( _, Nothing ) ->
                    ( ( score, idGen ), ( Just cell, processed ) )

                ( NumCell id num _, Just ((NumCell id2 num2 _) as unprocessed) ) ->
                    if num == num2 then
                        let
                            mergedNum =
                                num + num2

                            ( mergedCell, nextIdGen ) =
                                newCell mergedNum (MergedCell id id2) idGen

                            updatedScore =
                                score + mergedNum
                        in
                        ( ( updatedScore, nextIdGen ), ( Nothing, mergedCell :: processed ) )

                    else
                        ( ( score, idGen ), ( Just cell, unprocessed :: processed ) )

        unprocessedTupleToCellList ( maybeUnprocessed, processed ) =
            (case maybeUnprocessed of
                Just head ->
                    head :: processed

                Nothing ->
                    processed
            )
                |> cellListPadLeft

        compactAccToReturn ( acc, unprocessedTuple ) =
            ( acc
            , unprocessedTupleToCellList unprocessedTuple
            )
    in
    List.foldr func ( acc0, ( Nothing, [] ) )
        >> compactAccToReturn


cellListPadLeft : CellList -> CellList
cellListPadLeft l =
    List.repeat (4 - List.length l) EmptyCell ++ l
