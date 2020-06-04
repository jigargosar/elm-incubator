module MGrid exposing
    ( MGrid
    , Slot(..)
    , empty
    , fill
    , set
    , setAll
    , setAllEntriesBy
    , setEntry
    , setMaybeEntry
    , toList
    , viewFilledList
    , viewList
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
        |> List.foldl (\k -> Dict.insert k Empty) Dict.empty
        |> MGrid dimension_


setAll : List ( Location, a ) -> MGrid a -> MGrid a
setAll list g =
    List.foldl (uncurry set) g list


setMaybeEntry : Maybe ( Location, a ) -> MGrid a -> MGrid a
setMaybeEntry mx =
    case mx of
        Just x ->
            setEntry x

        Nothing ->
            identity


setAllEntriesBy : (a -> ( Location, b )) -> List a -> MGrid b -> MGrid b
setAllEntriesBy f xs g =
    List.foldl (f >> setEntry) g xs


mapDict : (Dict a -> Dict b) -> MGrid a -> MGrid b
mapDict f (MGrid dimension_ dict) =
    MGrid dimension_ (f dict)


set : Location -> a -> MGrid a -> MGrid a
set location a =
    mapDict
        (Dict.update (Location.toTuple location) (Maybe.map (\_ -> Filled a)))


setEntry : ( Location, a ) -> MGrid a -> MGrid a
setEntry ( location, a ) =
    set location a


fill : List Location -> a -> MGrid a -> MGrid a
fill locations a grid =
    List.foldl (\p -> set p a) grid locations


toList : MGrid a -> List ( Location, Slot a )
toList (MGrid _ dict) =
    Dict.toList dict |> List.map (Tuple.mapFirst Location.fromTuple)


viewList : (Location -> Slot a -> b) -> MGrid a -> List b
viewList f (MGrid _ dict) =
    Dict.foldr (\k -> f (Location.fromTuple k) >> (::)) [] dict


viewFilledList : (Location -> a -> b) -> MGrid a -> List b
viewFilledList f (MGrid _ dict) =
    Dict.foldr
        (\k v xs ->
            case v of
                Empty ->
                    xs

                Filled a ->
                    f (Location.fromTuple k) a :: xs
        )
        []
        dict


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
