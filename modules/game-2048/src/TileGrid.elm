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
                            newCell num idGen |> swap
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
    = Cell CellId Int
    | EmptyCell


newCell : Int -> CellIdGenerator -> ( Cell, CellIdGenerator )
newCell num =
    newCellId >> Tuple.mapFirst (\id -> Cell id num)
