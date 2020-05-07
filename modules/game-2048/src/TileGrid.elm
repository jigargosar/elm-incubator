module TileGrid exposing (TileGrid)

import Grid


type TileGrid
    = TileGrid (Grid.Grid Cell)



-- CellId


type CellId
    = CellId String


type CellIdGenerator
    = CellIdGenerator Int


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
