module Board exposing
    ( Board
    , Cell
    , Info
    , Msg(..)
    , info
    , init
    , update
    )

import Basics.Extra exposing (flip, swap, uncurry)
import Cons exposing (Cons)
import Dict exposing (Dict)
import Dict.Extra as Dict
import IncId exposing (IncId)
import IntPos exposing (IntPos)
import IntSize
import List.Extra as List
import PosDict exposing (PosDict)
import Random
import Random.List
import Tuple exposing (first, mapFirst, second)



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
    { entries = IncId.dictValues cellGrid.entriesById
    , newIds = cellGrid.newIds
    , newMergedIds = cellGrid.newMergedIds
    , mergedEntries = cellGrid.mergedEntries
    , removedIds = cellGrid.removedIds
    }


type Msg
    = SlideLeft
    | SlideRight
    | SlideUp
    | SlideDown


update : Msg -> Board -> Board
update msg ((Board cellGrid) as board) =
    updateCellGrid msg cellGrid
        |> Maybe.map Board
        |> Maybe.withDefault board


updateCellGrid : Msg -> CellGrid -> Maybe CellGrid
updateCellGrid msg cellGrid =
    slideCellGrid2 msg cellGrid
        |> fillRandomEmpty


slideCellGrid : Msg -> CellGrid -> CellGrid
slideCellGrid msg =
    case msg of
        SlideLeft ->
            slideBy PosDict.mapAccumFlippedRows

        SlideRight ->
            slideBy PosDict.mapAccumRows

        SlideUp ->
            slideBy PosDict.mapAccumFlippedColumns

        SlideDown ->
            slideBy PosDict.mapAccumColumns


slideCellGrid2 : Msg -> CellGrid -> CellGrid
slideCellGrid2 msg cellGrid =
    let
        slotsEntries =
            slideCellEntries msg (IncId.dictValues cellGrid.entriesById)

        acc =
            accumulateSlotEntries cellGrid.idSeed slotsEntries
    in
    { cellGrid
        | idSeed = acc.idSeed
        , entriesById = IncId.dictFromListBy (second >> .id) acc.entries
        , newMergedIds = acc.newMergedIds
        , mergedEntries = acc.mergedEntries
        , removedIds = (cellGrid.mergedEntries |> List.map (second >> .id)) ++ cellGrid.removedIds
    }


type alias Acc =
    { idSeed : IncId.Seed
    , entries : List ( IntPos, Cell )
    , newMergedIds : List IncId
    , mergedEntries : List ( IntPos, Cell )
    }


accumulateSlotEntries : IncId.Seed -> List ( IntPos, SlotResponse ) -> Acc
accumulateSlotEntries =
    let
        reducer ( pos, slot ) acc =
            case slot of
                Existing cell ->
                    { acc | entries = ( pos, cell ) :: acc.entries }

                EmptySlot ->
                    acc

                Merged c1 c2 ->
                    let
                        ( mergedCell, idSeed ) =
                            newCell (c1.num + c2.num) acc.idSeed
                    in
                    { acc
                        | idSeed = idSeed
                        , entries = ( pos, mergedCell ) :: acc.entries
                        , newMergedIds = mergedCell.id :: acc.newMergedIds
                        , mergedEntries = ( pos, c1 ) :: ( pos, c2 ) :: acc.mergedEntries
                    }
    in
    \idSeed ->
        List.foldl reducer
            { idSeed = idSeed
            , entries = []
            , newMergedIds = []
            , mergedEntries = []
            }



-- CELL


type alias Cell =
    { id : IncId
    , num : Int
    }


newCell : Int -> IncId.Seed -> ( Cell, IncId.Seed )
newCell num generator =
    let
        initCell : IncId -> Cell
        initCell id =
            { id = id, num = num }
    in
    IncId.next generator
        |> mapFirst initCell


newCells : List Int -> IncId.Seed -> ( List Cell, IncId.Seed )
newCells numList initialSeed =
    let
        reducer seed num =
            newCell num seed |> swap
    in
    List.mapAccuml reducer initialSeed numList
        |> swap



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


toCellDict : PosDict Slot -> PosDict Cell
toCellDict =
    Dict.filterMap (\_ -> toCell)


toSlotDict : PosDict.EntryList Cell -> PosDict Slot
toSlotDict =
    List.foldl (Tuple.mapSecond Filled >> PosDict.insertEntry)
        (PosDict.filled Empty size)


entriesByIdToSlotDict : IncId.IdDict (PosDict.Entry Cell) -> PosDict Slot
entriesByIdToSlotDict =
    IncId.dictValues >> toSlotDict



-- CELL GRID


type alias CellGrid =
    { idSeed : IncId.Seed
    , seed : Random.Seed
    , entriesById : IncId.IdDict (PosDict.Entry Cell)
    , mergedEntries : PosDict.EntryList Cell
    , removedIds : List IncId
    , newIds : List IncId
    , newMergedIds : List IncId
    }


size =
    IntSize.new 4 4


initialCellGrid : CellGrid
initialCellGrid =
    let
        ( ( cellEntries, idSeed ), seed ) =
            Random.step initialCellEntriesGenerator (Random.initialSeed 0)
    in
    { idSeed = idSeed
    , seed = seed
    , entriesById = IncId.dictFromListBy (second >> .id) cellEntries
    , mergedEntries = []
    , removedIds = []
    , newIds = []
    , newMergedIds = []
    }


initialPositionsGenerator : Random.Generator (List IntPos)
initialPositionsGenerator =
    Random.List.choose (IntSize.positions size)
        |> Random.andThen
            (\( p1, ps ) ->
                Random.List.choose ps
                    |> Random.map
                        (\( p2, _ ) ->
                            List.filterMap identity [ p1, p2 ]
                        )
            )


numGenerator : Random.Generator Int
numGenerator =
    Random.uniform 2 [ 4 ]


initialCellEntriesGenerator : Random.Generator ( PosDict.EntryList Cell, IncId.Seed )
initialCellEntriesGenerator =
    Random.map2
        (\ps ns ->
            newCells ns IncId.initialSeed
                |> Tuple.mapFirst (List.zip ps)
        )
        initialPositionsGenerator
        initialNumGenerator


initialNumGenerator : Random.Generator (List Int)
initialNumGenerator =
    Random.list 2 numGenerator



-- Slide


slideBy :
    ((SlideAcc -> List Slot -> ( SlideAcc, List Slot ))
     -> SlideAcc
     -> PosDict Slot
     -> ( SlideAcc, PosDict Slot )
    )
    -> CellGrid
    -> CellGrid
slideBy func cellGrid =
    let
        ( acc, dict ) =
            cellGrid.entriesById
                |> IncId.dictValues
                |> toSlotDict
                |> func compactSlotsRight (initSlideAcc cellGrid.idSeed)

        compactSlotsRight : SlideAcc -> List Slot -> ( SlideAcc, List Slot )
        compactSlotsRight slideAcc =
            List.foldr compactSlotReducer (initCompactAcc slideAcc)
                >> compactAccToReturn
    in
    cellGrid
        |> updateFromSlideResponse acc dict


type alias SlideAcc =
    { idSeed : IncId.Seed
    , mergedIdPairs : List ( IncId, IncId )
    }


initSlideAcc : IncId.Seed -> SlideAcc
initSlideAcc generator =
    { idSeed = generator
    , mergedIdPairs = []
    }


updateFromSlideResponse : SlideAcc -> PosDict Slot -> CellGrid -> CellGrid
updateFromSlideResponse acc dict cellGrid =
    let
        mergedIdPairToCellEntry : ( IncId, IncId ) -> Maybe (PosDict.Entry Cell)
        mergedIdPairToCellEntry ( fromId, toId ) =
            Maybe.map2
                (\( _, oldCell ) ( newPos, _ ) ->
                    ( newPos, oldCell )
                )
                (IncId.dictGet fromId cellGrid.entriesById)
                (IncId.dictGet toId newEntriesById)

        newEntriesById =
            toCellDict dict |> Dict.toList |> IncId.dictFromListBy (second >> .id)
    in
    { cellGrid
        | entriesById = newEntriesById
        , idSeed = acc.idSeed
        , newIds = []
        , newMergedIds = acc.mergedIdPairs |> List.map Tuple.second |> List.uniqueBy IncId.toInt
        , mergedEntries = acc.mergedIdPairs |> List.filterMap mergedIdPairToCellEntry
        , removedIds = cellGrid.mergedEntries |> List.map (second >> .id) |> (++) cellGrid.removedIds
    }


fillRandomEmpty : CellGrid -> Maybe CellGrid
fillRandomEmpty cellGrid =
    entriesByIdToSlotDict cellGrid.entriesById
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

        ( cell, idSeed ) =
            newCell num cellGrid.idSeed
    in
    { cellGrid
        | entriesById = IncId.dictInsert cell.id ( pos, cell ) cellGrid.entriesById
        , seed = seed
        , idSeed = idSeed
        , newIds = [ cell.id ]
    }



-- COMPACT ACC AND REDUCER


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

                    ( mergedCell, idSeed ) =
                        newCell (cell.num + prevCell.num) slideAcc.idSeed
                in
                { acc
                    | padCount = acc.padCount + 1
                    , processed = mergedCell :: acc.processed
                    , unprocessed = Nothing
                    , slideAcc =
                        { slideAcc
                            | idSeed = idSeed
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



-- Compact Logic v2


type SlotResponse
    = Existing Cell
    | EmptySlot
    | Merged Cell Cell


slideCellEntries : Msg -> List ( IntPos, Cell ) -> List ( IntPos, SlotResponse )
slideCellEntries msg entries =
    case msg of
        SlideLeft ->
            entries
                |> toRowLists
                |> List.map (List.reverse >> compactCellsRight >> List.reverse)
                |> fromRowLists

        SlideRight ->
            entries
                |> toRowLists
                |> List.map compactCellsRight
                |> fromRowLists

        SlideUp ->
            toColumnLists entries
                |> List.map (List.reverse >> compactCellsRight >> List.reverse)
                |> fromColumnLists

        SlideDown ->
            toColumnLists entries
                |> List.map compactCellsRight
                |> fromColumnLists



-- Entries Row Column Transforms


toRowLists : PosDict.EntryList a -> List (List a)
toRowLists =
    List.gatherEqualsBy (first >> second)
        >> List.map (Cons.toList >> List.sortBy first >> List.map second)


toColumnLists : PosDict.EntryList a -> List (List a)
toColumnLists =
    List.gatherEqualsBy (first >> first)
        >> List.map (Cons.toList >> List.sortBy first >> List.map second)


fromRowLists : List (List a) -> PosDict.EntryList a
fromRowLists =
    List.indexedMap
        (\y ->
            List.indexedMap (\x a -> ( ( x, y ), a ))
        )
        >> List.concat


fromColumnLists : List (List a) -> PosDict.EntryList a
fromColumnLists =
    List.indexedMap
        (\x ->
            List.indexedMap (\y a -> ( ( x, y ), a ))
        )
        >> List.concat



-- CompactCellsToSlots


compactCellsRight : List Cell -> List SlotResponse
compactCellsRight =
    let
        reducer cell ( mx, xs ) =
            case mx of
                Nothing ->
                    ( Just cell, xs )

                Just unprocessed ->
                    if cell.num == unprocessed.num then
                        ( Nothing, Merged unprocessed cell :: xs )

                    else
                        ( Just cell, Existing unprocessed :: xs )
    in
    List.foldr reducer ( Nothing, [] )
        >> mapFirst (Maybe.map Existing)
        >> uncurry consMaybe
        >> gridSlotsPadLeft


gridSlotsPadLeft : List SlotResponse -> List SlotResponse
gridSlotsPadLeft list =
    let
        len =
            List.length list
    in
    List.repeat (4 - len) EmptySlot ++ list
