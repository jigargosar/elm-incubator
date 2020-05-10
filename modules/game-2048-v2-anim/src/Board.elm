module Board exposing
    ( Cell
    , CellGrid
    , Info
    , info
    , initialCellGrid
    , updateCellGrid
    )

import Basics.Extra exposing (flip, uncurry)
import Cons exposing (Cons)
import Dict exposing (Dict)
import Dict.Extra as Dict
import IncId exposing (IncId)
import IntPos exposing (IntPos)
import IntSize
import List.Extra as List
import PosDict exposing (PosDict)
import Random
import Tuple exposing (first, mapFirst, mapSecond, pair, second)


type Slot
    = Filled Cell
    | Empty


cellAt : IntPos -> PosDict Slot -> Maybe Cell
cellAt pos =
    Dict.get pos >> Maybe.andThen toCell


toCell : Slot -> Maybe Cell
toCell slot =
    case slot of
        Filled cell ->
            Just cell

        Empty ->
            Nothing


toCellPosDict : PosDict Slot -> PosDict Cell
toCellPosDict =
    Dict.filterMap (\_ -> toCell)


type alias Cell =
    { id : IncId
    , num : Int
    }


newCell : Int -> IncId.Generator -> ( Cell, IncId.Generator )
newCell num generator =
    let
        initCell : IncId -> Cell
        initCell id =
            { id = id, num = num }
    in
    IncId.new generator
        |> mapFirst initCell


type alias CellGrid =
    { idGenerator : IncId.Generator
    , seed : Random.Seed
    , dict : PosDict Slot
    , mergedEntries : PosDict.EntryList Cell
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
    , seed = Random.initialSeed 0
    , dict =
        PosDict.filled Empty size
            |> Dict.insert ( 1, 1 ) (Filled cell1)
            |> Dict.insert ( 2, 2 ) (Filled cell2)
    , mergedEntries = []
    , generatedIds = []
    , step = 0
    }


type alias Info =
    { entries : PosDict.EntryList Cell
    , generatedIds : List IncId
    , mergedEntries : PosDict.EntryList Cell
    }


info : CellGrid -> Info
info cellGrid =
    { entries =
        cellGrid.dict
            |> toCellPosDict
            |> Dict.toList
    , generatedIds = cellGrid.generatedIds
    , mergedEntries = cellGrid.mergedEntries
    }


type alias SlideAcc =
    { idGenerator : IncId.Generator
    , mergedIdPairs : List ( IncId, IncId )
    }


initSlideAcc : IncId.Generator -> SlideAcc
initSlideAcc generator =
    { idGenerator = generator
    , mergedIdPairs = []
    }


slideRight : CellGrid -> CellGrid
slideRight cellGrid =
    let
        ( acc, dict ) =
            PosDict.mapAccumRowsR slotListCompactRight (initSlideAcc cellGrid.idGenerator) cellGrid.dict

        oldCellPosDict =
            toCellPosDict cellGrid.dict

        newCellPosDict =
            toCellPosDict dict

        mergedIdPairToCellEntry : ( IncId, IncId ) -> Maybe (PosDict.Entry Cell)
        mergedIdPairToCellEntry ( fromId, toId ) =
            Maybe.map2
                (\( _, oldCell ) ( newPos, _ ) ->
                    ( newPos, oldCell )
                )
                (Dict.find (\_ cell -> cell.id == fromId) oldCellPosDict)
                (Dict.find (\_ cell -> cell.id == toId) newCellPosDict)
    in
    { cellGrid
        | dict = dict
        , idGenerator = acc.idGenerator
        , generatedIds = []
        , mergedEntries = acc.mergedIdPairs |> List.filterMap mergedIdPairToCellEntry
    }


fillRandomEmpty : CellGrid -> Maybe CellGrid
fillRandomEmpty cellGrid =
    cellGrid.dict
        |> Dict.filter (\_ slot -> slot == Empty)
        |> Dict.keys
        |> Cons.fromList
        |> Maybe.map (flip fillRandomPosition cellGrid)


fillRandomPosition : Cons IntPos -> CellGrid -> CellGrid
fillRandomPosition ( h, t ) cellGrid =
    let
        ( ( pos, num ), seed ) =
            Random.step
                (Random.pair
                    (Random.uniform h t)
                    (Random.uniform 2 [ 4 ])
                )
                cellGrid.seed

        ( cell, idGenerator ) =
            newCell num cellGrid.idGenerator
    in
    { cellGrid
        | dict = Dict.insert pos (Filled cell) cellGrid.dict
        , seed = seed
        , idGenerator = idGenerator
        , generatedIds = [ cell.id ]
    }



-- COMPACT ACC AND REDUCER


slotListCompactRight : SlideAcc -> List Slot -> ( SlideAcc, List Slot )
slotListCompactRight slideAcc slots =
    slots
        |> List.foldr compactSlotReducer (initCompactAcc slideAcc)
        |> compactAccToReturn


type alias CompactAcc =
    { slideAcc : SlideAcc
    , unprocessed : Maybe Cell
    , processed : List Cell
    , padCount : Int
    }


initCompactAcc : SlideAcc -> CompactAcc
initCompactAcc slideAcc =
    { slideAcc = slideAcc, unprocessed = Nothing, processed = [], padCount = 0 }


consMaybe : Maybe a -> List a -> List a
consMaybe mx xs =
    case mx of
        Just x ->
            x :: xs

        Nothing ->
            xs


compactAccToReturn : CompactAcc -> ( SlideAcc, List Slot )
compactAccToReturn acc =
    let
        slots : List Slot
        slots =
            List.repeat acc.padCount Empty
                ++ List.map Filled (consMaybe acc.unprocessed acc.processed)

        _ =
            if List.length slots /= 4 then
                Debug.todo "List.length slots /= 4"

            else
                never
    in
    ( acc.slideAcc, slots )


compactSlotReducer : Slot -> CompactAcc -> CompactAcc
compactSlotReducer slot acc =
    case ( slot, acc.unprocessed ) of
        ( Empty, _ ) ->
            { acc | padCount = acc.padCount + 1 }

        ( Filled cell, Nothing ) ->
            { acc | unprocessed = Just cell }

        ( Filled cell, Just prevCell ) ->
            if cell.num == prevCell.num then
                let
                    slideAcc =
                        acc.slideAcc

                    ( mergedCell, idGenerator ) =
                        newCell (cell.num + prevCell.num) slideAcc.idGenerator
                in
                { acc
                    | padCount = acc.padCount + 1
                    , processed = mergedCell :: acc.processed
                    , unprocessed = Nothing
                    , slideAcc =
                        { slideAcc
                            | idGenerator = idGenerator
                            , mergedIdPairs =
                                ( cell.id, mergedCell.id )
                                    :: ( prevCell.id, mergedCell.id )
                                    :: slideAcc.mergedIdPairs
                        }
                }

            else
                { acc
                    | processed = prevCell :: acc.processed
                    , unprocessed = Just cell
                }


updateCellGrid : CellGrid -> CellGrid
updateCellGrid cellGrid0 =
    case
        cellGrid0.step
    of
        0 ->
            let
                cellGrid1 =
                    slideRight cellGrid0
            in
            let
                nextDict =
                    cellGrid1.dict
                        |> Dict.insert ( 2, 1 ) (Filled generatedCell)

                ( generatedCell, nextIdGenerator ) =
                    newCell 2 cellGrid1.idGenerator

                nextGenerated =
                    [ generatedCell.id ]
            in
            { cellGrid1
                | dict = nextDict
                , idGenerator = nextIdGenerator
                , generatedIds = nextGenerated

                --, mergedEntries = []
                , step = cellGrid1.step + 1
            }

        1 ->
            let
                mergedEntries =
                    Maybe.map2
                        (\c1 c2 ->
                            [ ( ( 0, 1 ), c1 )
                            , ( ( 0, 1 ), c2 )
                            ]
                        )
                        (cellAt ( 2, 1 ) cellGrid0.dict)
                        (cellAt ( 3, 1 ) cellGrid0.dict)
                        |> Maybe.withDefault []

                nextDict =
                    cellGrid0.dict
                        |> Dict.remove ( 2, 1 )
                        |> Dict.remove ( 3, 1 )
                        |> PosDict.swap ( 3, 2 ) ( 0, 2 )
                        |> Dict.insert ( 0, 1 ) (Filled mergedCell)
                        |> Dict.insert ( 1, 1 ) (Filled generatedCell)

                ( mergedCell, idGen0 ) =
                    newCell 4 cellGrid0.idGenerator

                ( generatedCell, idGen1 ) =
                    newCell 2 idGen0

                nextGenerated =
                    [ generatedCell.id ]
            in
            { cellGrid0
                | dict = nextDict
                , generatedIds = nextGenerated
                , idGenerator = idGen1
                , mergedEntries = mergedEntries
                , step = cellGrid0.step + 1
            }

        2 ->
            { cellGrid0
                | idGenerator = IncId.newGenerator
                , dict = Dict.empty
                , generatedIds = []
                , mergedEntries = []
                , step = cellGrid0.step + 1
            }

        _ ->
            initialCellGrid
