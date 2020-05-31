module MGrid exposing
    ( MGrid
    , Slot(..)
    , adjacent
    , dimension
    , empty
    , fill
    , fillWhenEmpty
    , map
    , maybeFilledAt
    , set
    , setAll
    , setWhenEmpty
    , slotAt
    , viewRows
    )

import Basics.Extra exposing (flip, uncurry)
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


slotAt : Location -> MGrid a -> Maybe (Slot a)
slotAt location (MGrid _ d) =
    Dict.get (Location.toTuple location) d


maybeFilledAt : Location -> MGrid a -> Maybe (Maybe a)
maybeFilledAt location g =
    slotAt location g
        |> Maybe.map
            (\slot ->
                case slot of
                    Filled a ->
                        Just a

                    Empty ->
                        Nothing
            )


fill : List Location -> a -> MGrid a -> MGrid a
fill locations a grid =
    List.foldl (\p -> set p a) grid locations


adjacent : Location -> MGrid a -> List ( Location, Slot a )
adjacent location (MGrid _ d) =
    Location.adjacent location
        |> List.foldl
            (\p acc ->
                case Dict.get (Location.toTuple p) d of
                    Nothing ->
                        acc

                    Just a ->
                        ( p, a ) :: acc
            )
            []


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


dimension : MGrid a -> Dimension
dimension (MGrid dimension_ _) =
    dimension_


map : (Location -> a -> b) -> MGrid a -> MGrid b
map f =
    mapDict
        (Dict.map
            (\i2 slot ->
                case slot of
                    Filled a ->
                        Filled (f (Location.fromTuple i2) a)

                    Empty ->
                        Empty
            )
        )


fillWhenEmpty : List Location -> b -> MGrid b -> MGrid b
fillWhenEmpty locations a grid =
    List.foldl (flip setWhenEmpty a) grid locations


setWhenEmpty : Location -> b -> MGrid b -> MGrid b
setWhenEmpty location a =
    mapDict
        (Dict.update (Location.toTuple location)
            (Maybe.map
                (\slot ->
                    case slot of
                        Filled _ ->
                            slot

                        Empty ->
                            Filled a
                )
            )
        )
