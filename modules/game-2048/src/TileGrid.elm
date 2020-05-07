module TileGrid exposing (TileGrid)

import Grid
import List.Extra
import PosDict


type TileGrid
    = TileGrid (Grid.Grid Cell)


size =
    { width = 4, height = 4 }


fromNumRows : List (List Int) -> TileGrid
fromNumRows lists =
    lists
        |> List.Extra.mapAccuml
            (\acc0 row ->
                List.Extra.mapAccuml (\acc num -> ( acc, Cell (CellId "0") num )) acc0 row
            )
            initCellIdGenerator
        |> Tuple.second
        |> Grid.fromRowLists size (Cell (CellId "0") 0)
        |> TileGrid



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


type alias Cell =
    { id : CellId
    , num : Int
    }


newCell : Int -> CellIdGenerator -> ( Cell, CellIdGenerator )
newCell num =
    newCellId >> Tuple.mapFirst (\id -> Cell id num)
