module Grid exposing (Grid, Slot(..), fromEntries)

import IntSize exposing (IntSize)
import PosDict exposing (EntryList, PosDict)
import Tuple exposing (mapSecond)


type Slot a
    = Empty
    | Filled a


type alias Dict a =
    PosDict (Slot a)


type Grid a
    = Grid (Dict a)


fromEntries : IntSize -> EntryList a -> Grid a
fromEntries s xs =
    PosDict.filled Empty s
        |> PosDict.insertAll (List.map (mapSecond Filled) xs)
        |> Grid
