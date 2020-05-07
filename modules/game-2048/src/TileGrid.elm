module TileGrid exposing (TileGrid, fromNumRows)

import Basics.Extra exposing (swap, uncurry)
import Grid
import List.Extra


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



-- Cell List


type alias CellList =
    List Cell


type alias CompactAcc =
    ( Int, ( ( Maybe Cell, CellList ), CellIdGenerator ) )


cellListCompactRight : CellIdGenerator -> CellList -> ( Int, ( CellList, CellIdGenerator ) )
cellListCompactRight idGen0 =
    let
        func : Cell -> CompactAcc -> CompactAcc
        func cell (( score, ( ( maybeUnprocessed, processed ), idGen ) ) as acc) =
            case ( cell, maybeUnprocessed ) of
                ( EmptyCell, _ ) ->
                    acc

                ( _, Nothing ) ->
                    ( score, ( ( Just cell, processed ), idGen ) )

                ( NumCell id num _, Just ((NumCell id2 num2 _) as unprocessed) ) ->
                    if num == num2 then
                        let
                            mergedNum =
                                num * 2

                            ( mergedCell, nextIdGen ) =
                                newCell mergedNum (MergedCell id id2) idGen

                            updatedScore =
                                score + mergedNum
                        in
                        ( updatedScore, ( ( Nothing, mergedCell :: processed ), nextIdGen ) )

                    else
                        ( score, ( ( Just cell, unprocessed :: processed ), idGen ) )

        unprocessedTupleToList ( maybeUnprocessed, processed ) =
            case maybeUnprocessed of
                Just head ->
                    head :: processed

                Nothing ->
                    processed
    in
    List.foldr func ( 0, ( ( Nothing, [] ), idGen0 ) )
        >> Tuple.mapSecond
            (Tuple.mapFirst (unprocessedTupleToList >> cellListPadLeft))


cellListPadLeft : CellList -> CellList
cellListPadLeft l =
    List.repeat (4 - List.length l) EmptyCell ++ l
