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
import IncId exposing (IncId)
import IntPos exposing (IntPos)
import IntSize
import List.Extra as List
import PosDict exposing (PosDict)
import Random
import Random.List
import Tuple exposing (..)



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
    slideCellGrid msg cellGrid
        |> fillRandomEmpty
        |> Maybe.map Board
        |> Maybe.withDefault board


slideCellGrid : Msg -> CellGrid -> CellGrid
slideCellGrid msg cellGrid =
    let
        acc =
            toSlotDict cellGrid.entriesById
                |> slideSlotEntries msg
                |> accumulateSlideResponse cellGrid.idSeed
    in
    { cellGrid
        | idSeed = acc.idSeed
        , entriesById = IncId.dictFromListBy (second >> .id) acc.entries
        , newMergedIds = acc.newMergedIds
        , mergedEntries = acc.mergedEntries
        , removedIds = (cellGrid.mergedEntries |> List.map (second >> .id)) ++ cellGrid.removedIds
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


fillRandomEmpty : CellGrid -> Maybe CellGrid
fillRandomEmpty cellGrid =
    toSlotDict cellGrid.entriesById
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



-- ACCUMULATE SLOTS


type alias SlideResponse =
    { idSeed : IncId.Seed
    , entries : List ( IntPos, Cell )
    , newMergedIds : List IncId
    , mergedEntries : List ( IntPos, Cell )
    }


accumulateSlideResponse : IncId.Seed -> PosDict SlotResponse -> SlideResponse
accumulateSlideResponse =
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
        Dict.toList
            >> List.foldl reducer
                { idSeed = idSeed
                , entries = []
                , newMergedIds = []
                , mergedEntries = []
                }



-- SLIDE SLOTS


type Slot
    = Filled Cell
    | Empty


toSlotDict : IncId.IdDict (PosDict.Entry Cell) -> PosDict Slot
toSlotDict =
    IncId.dictValues
        >> List.map (mapSecond Filled)
        >> flip PosDict.insertAll (PosDict.filled Empty size)


consMaybe : Maybe a -> List a -> List a
consMaybe mx xs =
    case mx of
        Just x ->
            x :: xs

        Nothing ->
            xs


type SlotResponse
    = Existing Cell
    | EmptySlot
    | Merged Cell Cell


slideSlotEntries : Msg -> PosDict Slot -> PosDict SlotResponse
slideSlotEntries msg entries =
    case msg of
        SlideLeft ->
            entries
                |> PosDict.toRows
                |> List.map (List.reverse >> compactSlotsRight >> List.reverse)
                |> PosDict.fromRows

        SlideRight ->
            entries
                |> PosDict.toRows
                |> List.map compactSlotsRight
                |> PosDict.fromRows

        SlideUp ->
            entries
                |> PosDict.toColumns
                |> List.map (List.reverse >> compactSlotsRight >> List.reverse)
                |> PosDict.fromColumns

        SlideDown ->
            entries
                |> PosDict.toColumns
                |> List.map compactSlotsRight
                |> PosDict.fromColumns


compactSlotsRight : List Slot -> List SlotResponse
compactSlotsRight =
    let
        reducer slot ( mx, xs ) =
            case ( slot, mx ) of
                ( Empty, _ ) ->
                    ( mx, xs )

                ( Filled cell, Nothing ) ->
                    ( Just cell, xs )

                ( Filled cell, Just unprocessed ) ->
                    if cell.num == unprocessed.num then
                        ( Nothing, Merged unprocessed cell :: xs )

                    else
                        ( Just cell, Existing unprocessed :: xs )
    in
    List.foldr reducer ( Nothing, [] )
        >> mapFirst (Maybe.map Existing)
        >> uncurry consMaybe
        >> slotsResponsePadLeft


slotsResponsePadLeft : List SlotResponse -> List SlotResponse
slotsResponsePadLeft list =
    let
        len =
            List.length list
    in
    List.repeat (size.width - len) EmptySlot ++ list
