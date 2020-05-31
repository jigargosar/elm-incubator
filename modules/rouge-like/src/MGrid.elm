module MGrid exposing
    ( MGrid
    , Slot(..)
    , empty
    , fill
    , set
    , setAll
    , viewRows
    )

import Basics.Extra exposing (uncurry)
import Dict
import Dimension exposing (Dimension)
import Location exposing (Location)


type alias Dict a =
    Dict.Dict ( Int, Int ) (Slot a)


type Slot a
    = Empty
    | Filled a


type MGrid a
    = MGrid Dimension (Dict a)


empty : Dimension -> MGrid a
empty dimension_ =
    Dimension.toLocations dimension_
        |> List.map Location.toTuple
        |> List.foldl (\p -> Dict.insert p Empty) Dict.empty
        |> MGrid dimension_


setAll : List ( Location, a ) -> MGrid a -> MGrid a
setAll list g =
    List.foldl (uncurry set) g list


mapDict : (Dict a -> Dict b) -> MGrid a -> MGrid b
mapDict f (MGrid dimension_ dict) =
    MGrid dimension_ (f dict)


set : Location -> a -> MGrid a -> MGrid a
set location a =
    mapDict
        (Dict.update (Location.toTuple location) (Maybe.map (\_ -> Filled a)))


fill : List Location -> a -> MGrid a -> MGrid a
fill locations a grid =
    List.foldl (\p -> set p a) grid locations


toEntryRows : MGrid a -> List (List ( Location, Slot a ))
toEntryRows (MGrid dimension_ dict) =
    Dimension.toLocationRows dimension_
        |> List.map
            (List.filterMap
                (\p ->
                    Dict.get (Location.toTuple p) dict
                        |> Maybe.map (\a -> ( p, a ))
                )
            )


viewRows : (Int -> List a -> b) -> (Location -> Slot c -> a) -> MGrid c -> List b
viewRows rf ef =
    toEntryRows
        >> List.indexedMap
            (\row rowEntries ->
                List.map (\( p, s ) -> ef p s) rowEntries
                    |> rf row
            )
