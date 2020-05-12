module Grid exposing (Grid, Slot(..), fromEntries, fromRows, toRows)

import IntSize exposing (IntSize)
import PosDict exposing (EntryList, PosDict)
import Tuple exposing (mapSecond)


type Slot a
    = Empty
    | Filled a


type Grid a
    = Grid (PosDict (Slot a))


fromEntries : IntSize -> EntryList a -> Grid a
fromEntries s xs =
    PosDict.filled Empty s
        |> PosDict.insertAll (List.map (mapSecond Filled) xs)
        |> Grid


toRows : Grid a -> List (List (Slot a))
toRows (Grid d) =
    PosDict.toRows d


fromRows : IntSize -> List (List (Slot a)) -> Grid a
fromRows s rs =
    PosDict.fromRows rs
        |> PosDict.resize s Empty
        |> Grid
