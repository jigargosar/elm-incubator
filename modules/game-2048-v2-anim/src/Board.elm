module Board exposing
    ( Board
    , Cell
    , Info
    , info
    , init
    , update
    )

import Basics.Extra exposing (flip)
import Cons exposing (Cons)
import Dict exposing (Dict)
import Dict.Extra as Dict
import IncId exposing (IncId)
import IntPos exposing (IntPos)
import IntSize
import List.Extra as List
import PosDict exposing (PosDict)
import Random
import Tuple exposing (mapFirst, second)



-- BOARD


type Board
    = Board CellGrid


init : Board
init =
    Board initialCellGrid


type alias Info =
    { entries : PosDict.EntryList Cell
    , newIds : List IncId
    , newMergedIds : List IncId
    , mergedEntries : PosDict.EntryList Cell
    , removedIds : List IncId
    }


info : Board -> Info
info (Board cellGrid) =
    { entries =
        cellGrid.dict
            |> toCellPosDict
            |> Dict.toList
    , newIds = cellGrid.newIds
    , newMergedIds = cellGrid.newMergedIds
    , mergedEntries = cellGrid.mergedEntries
    , removedIds = cellGrid.removedIds
    }


update : Board -> Board
update (Board cellGrid) =
    Board (updateCellGrid cellGrid)



-- CELL


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



-- SLOT


type Slot
    = Filled Cell
    | Empty


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



-- CELL GRID


type alias CellGrid =
    { idGenerator : IncId.Generator
    , seed : Random.Seed
    , dict : PosDict Slot
    , mergedEntries : PosDict.EntryList Cell
    , removedIds : List IncId
    , newIds : List IncId
    , newMergedIds : List IncId
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
    , removedIds = []
    , newIds = []
    , newMergedIds = []
    , step = 0
    }



-- UPDATE


updateCellGrid : CellGrid -> CellGrid
updateCellGrid cellGrid =
    case modBy 4 cellGrid.step of
        0 ->
            cellGrid
                |> incStep
                |> slideRight

        1 ->
            cellGrid
                |> incStep
                |> slideLeft

        2 ->
            cellGrid
                |> incStep
                |> slideUp

        _ ->
            cellGrid
                |> incStep
                |> slideDown


incStep : CellGrid -> CellGrid
incStep cellGrid =
    { cellGrid | step = cellGrid.step + 1 }



-- Slide


slideRight : CellGrid -> CellGrid
slideRight =
    slideWith PosDict.mapAccumRows


slideLeft : CellGrid -> CellGrid
slideLeft =
    slideWith PosDict.mapAccumFlippedRows


slideDown : CellGrid -> CellGrid
slideDown =
    slideWith PosDict.mapAccumColumns


slideUp : CellGrid -> CellGrid
slideUp =
    slideWith PosDict.mapAccumFlippedColumns


slideWith :
    ((SlideAcc -> List Slot -> ( SlideAcc, List Slot ))
     -> SlideAcc
     -> PosDict Slot
     -> ( SlideAcc, PosDict Slot )
    )
    -> CellGrid
    -> CellGrid
slideWith func cellGrid =
    let
        ( acc, dict ) =
            cellGrid.dict
                |> func compactSlotsRight (initSlideAcc cellGrid.idGenerator)
    in
    cellGrid
        |> updateFromSlideResponse acc dict
        |> fillRandomEmpty
        |> Maybe.withDefault cellGrid


type alias SlideAcc =
    { idGenerator : IncId.Generator
    , mergedIdPairs : List ( IncId, IncId )
    }


initSlideAcc : IncId.Generator -> SlideAcc
initSlideAcc generator =
    { idGenerator = generator
    , mergedIdPairs = []
    }


updateFromSlideResponse : SlideAcc -> PosDict Slot -> CellGrid -> CellGrid
updateFromSlideResponse acc dict cellGrid =
    let
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
        , newIds = []
        , newMergedIds = acc.mergedIdPairs |> List.map Tuple.second |> List.uniqueBy IncId.toInt
        , mergedEntries = acc.mergedIdPairs |> List.filterMap mergedIdPairToCellEntry
        , removedIds = cellGrid.mergedEntries |> List.map (second >> .id) |> (++) cellGrid.removedIds
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
        , newIds = [ cell.id ]
    }



-- COMPACT ACC AND REDUCER


compactSlotsRight : SlideAcc -> List Slot -> ( SlideAcc, List Slot )
compactSlotsRight slideAcc =
    List.foldr compactSlotReducer (initCompactAcc slideAcc)
        >> compactAccToReturn


compactSlotsLeft : SlideAcc -> List Slot -> ( SlideAcc, List Slot )
compactSlotsLeft slideAcc =
    List.foldl compactSlotReducer (initCompactAcc slideAcc)
        >> compactAccToReturn


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
