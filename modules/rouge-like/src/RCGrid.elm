module RCGrid exposing
    ( Grid
    , Slot(..)
    , adjacentSlotEntries
    , empty
    , fill
    , filledWith
    , set
    , setAll
    , toEntryRows
    )

import Basics.Extra exposing (flip)
import Dict
import Dimension exposing (Dimension)
import Position exposing (Position)
import RC exposing (RC)
import Tuple exposing (pair)


type alias Dict a =
    Dict.Dict RC (Slot a)


type Slot a
    = Empty
    | Filled a


type Grid a
    = Grid Dimension (Dict a)


filledWith : Dimension -> a -> Grid a
filledWith dimension a =
    filledWithSlot dimension (Filled a)


empty : Dimension -> Grid a
empty dimension =
    filledWithSlot dimension Empty


filledWithSlot : Dimension -> Slot a -> Grid a
filledWithSlot dimension slot =
    Dimension.toPositions dimension
        |> List.map Position.toTuple
        |> List.foldl (flip Dict.insert slot) Dict.empty
        |> Grid dimension


set : RC -> a -> Grid a -> Grid a
set rc a =
    mapDict
        (Dict.update rc (Maybe.map (\_ -> Filled a)))


setAll : List ( RC, a ) -> Grid a -> Grid a
setAll list g =
    List.foldl setEntry g list


setEntry : ( RC, a ) -> Grid a -> Grid a
setEntry ( rc, a ) grid =
    set rc a grid


slotEntryAt : RC -> Grid a -> Maybe ( RC, Slot a )
slotEntryAt rc (Grid _ d) =
    Dict.get rc d |> Maybe.map (pair rc)


mapDict : (Dict a -> Dict a) -> Grid a -> Grid a
mapDict f (Grid dimension dict) =
    Grid dimension (f dict)


fill : List RC -> a -> Grid a -> Grid a
fill rcList a grid =
    List.foldl (\p -> set p a) grid rcList


adjacentSlotEntries : RC -> Grid a -> List ( RC, Slot a )
adjacentSlotEntries rc g =
    RC.adjacent rc
        |> List.filterMap (flip slotEntryAt g)


toEntryRows : Grid a -> List (List ( Position, Slot a ))
toEntryRows (Grid dimension dict) =
    Dimension.toPositionRows dimension
        |> List.map
            (List.filterMap
                (\p ->
                    Dict.get (Position.toTuple p) dict
                        |> Maybe.map (\a -> ( p, a ))
                )
            )
