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
import Grid exposing (Grid, Slot(..))
import IncId exposing (IncId)
import IntPos exposing (IntPos)
import IntSize
import List.Extra as List
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
    { entries : Grid.EntryList Cell
    , newIds : List IncId
    , newMergedIds : List IncId
    , mergedEntries : Grid.EntryList Cell
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
            toGrid cellGrid.entriesById
                |> slideGrid msg
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
    , entriesById : IncId.IdDict (Grid.Entry Cell)
    , mergedEntries : Grid.EntryList Cell
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


initialCellEntriesGenerator : Random.Generator ( Grid.EntryList Cell, IncId.Seed )
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
    cellGrid.entriesById
        |> toGrid
        |> Grid.emptyPositions
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


accumulateSlideResponse : IncId.Seed -> Grid OutCell -> SlideResponse
accumulateSlideResponse =
    let
        reducer ( pos, slot ) acc =
            case slot of
                Existing cell ->
                    { acc | entries = ( pos, cell ) :: acc.entries }

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
        Grid.toEntries
            >> List.foldl reducer
                { idSeed = idSeed
                , entries = []
                , newMergedIds = []
                , mergedEntries = []
                }



-- SLIDE SLOTS


type OutCell
    = Existing Cell
    | Merged Cell Cell


toGrid : IncId.IdDict (Grid.Entry Cell) -> Grid Cell
toGrid =
    IncId.dictValues
        >> Grid.fromEntries size


slideGrid : Msg -> Grid Cell -> Grid OutCell
slideGrid msg entries =
    case msg of
        SlideLeft ->
            entries
                |> Grid.toRows
                |> List.map (List.reverse >> compactSlotsRight >> List.reverse)
                |> Grid.fromRows size

        SlideRight ->
            entries
                |> Grid.toRows
                |> List.map compactSlotsRight
                |> Grid.fromRows size

        SlideUp ->
            entries
                |> Grid.toColumns
                |> List.map (List.reverse >> compactSlotsRight >> List.reverse)
                |> Grid.fromColumns size

        SlideDown ->
            entries
                |> Grid.toColumns
                |> List.map compactSlotsRight
                |> Grid.fromColumns size


compactSlotsRight : List (Slot Cell) -> List (Slot OutCell)
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


consMaybe : Maybe a -> List a -> List a
consMaybe mx xs =
    case mx of
        Just x ->
            x :: xs

        Nothing ->
            xs


slotsResponsePadLeft : List a -> List (Slot a)
slotsResponsePadLeft list =
    let
        len =
            List.length list
    in
    List.repeat (size.width - len) Empty ++ List.map Filled list
