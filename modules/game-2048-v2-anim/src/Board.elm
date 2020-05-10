module Board exposing (Cell, CellGrid, Slot(..), initialCellGrid, updateCellGrid)

import Dict exposing (Dict)
import IncId exposing (IncId)
import IntPos exposing (IntPos)
import IntSize
import PosDict exposing (PosDict)
import Tuple exposing (mapFirst)



-- Cell


type Slot
    = Filled Cell
    | Empty


type alias Cell =
    { id : IncId
    , num : Int
    }


newCell : Int -> IncId.Generator -> ( Cell, IncId.Generator )
newCell num generator =
    let
        initCell id =
            Cell id num
    in
    IncId.new generator
        |> mapFirst initCell


type alias CellGrid =
    { idGenerator : IncId.Generator
    , dict : PosDict Slot
    , merged : List ( IntPos, Cell )
    , generatedIds : List IncId
    , step : Int
    }


size =
    IntSize.new 4 4


initialCellGrid : CellGrid
initialCellGrid =
    let
        idGen0 =
            IncId.newGenerator

        ( cell1, idGen1 ) =
            newCell 2 idGen0

        ( cell2, idGen2 ) =
            newCell 4 idGen1
    in
    { idGenerator = idGen2
    , dict =
        PosDict.filled Empty size
            |> Dict.insert ( 1, 1 ) (Filled cell1)
            |> Dict.insert ( 2, 2 ) (Filled cell2)
    , merged = []
    , generatedIds = []
    , step = 0
    }


updateCellGrid : CellGrid -> CellGrid
updateCellGrid cellGrid =
    case
        cellGrid.step
    of
        0 ->
            let
                nextDict =
                    cellGrid.dict
                        |> PosDict.swap ( 1, 1 ) ( 3, 1 )
                        |> PosDict.swap ( 2, 2 ) ( 3, 2 )
                        |> Dict.insert ( 2, 1 ) (Filled generatedCell)

                ( generatedCell, nextIdGenerator ) =
                    newCell 2 cellGrid.idGenerator

                nextGenerated =
                    [ generatedCell.id ]
            in
            { cellGrid
                | dict = nextDict
                , idGenerator = nextIdGenerator
                , generatedIds = nextGenerated
                , merged = []
                , step = cellGrid.step + 1
            }

        1 ->
            let
                foo =
                    Maybe.map2
                        (\s1 s2 ->
                            case ( s1, s2 ) of
                                ( Filled c1, Filled c2 ) ->
                                    [ ( ( 0, 1 ), c1 )
                                    , ( ( 0, 1 ), c2 )
                                    ]

                                _ ->
                                    []
                        )
                        (Dict.get ( 2, 1 ) cellGrid.dict)
                        (Dict.get ( 3, 1 ) cellGrid.dict)
                        |> Maybe.withDefault []

                nextDict =
                    cellGrid.dict
                        |> Dict.remove ( 2, 1 )
                        |> Dict.remove ( 3, 1 )
                        |> PosDict.swap ( 3, 2 ) ( 0, 2 )
                        |> Dict.insert ( 0, 1 ) (Filled mergedCell)
                        |> Dict.insert ( 1, 1 ) (Filled generatedCell)

                ( mergedCell, idGen0 ) =
                    newCell 4 cellGrid.idGenerator

                ( generatedCell, idGen1 ) =
                    newCell 2 idGen0

                nextGenerated =
                    [ generatedCell.id ]
            in
            { cellGrid
                | dict = nextDict
                , generatedIds = nextGenerated
                , idGenerator = idGen1
                , merged = foo
                , step = cellGrid.step + 1
            }

        2 ->
            { cellGrid
                | idGenerator = IncId.newGenerator
                , dict = Dict.empty
                , generatedIds = []
                , merged = []
                , step = cellGrid.step + 1
            }

        _ ->
            initialCellGrid
