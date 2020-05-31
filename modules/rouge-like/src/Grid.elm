module Grid exposing
    ( Grid
    , Slot(..)
    , adjacent
    , dimension
    , empty
    , fill
    , fillWhenEmpty
    , map
    , set
    , setAll
    , setWhenEmpty
    , slotAt
    , viewRows
    )

import Basics.Extra exposing (flip, uncurry)
import Dict
import Dimension exposing (Dimension)
import Position exposing (Position)


type alias Dict a =
    Dict.Dict ( Int, Int ) (Slot a)


type Slot a
    = Empty
    | Filled a


type Grid a
    = Grid Dimension (Dict a)


empty : Dimension -> Grid a
empty dimension_ =
    Dimension.toPositions dimension_
        |> List.map Position.toTuple
        |> List.foldl (\p -> Dict.insert p Empty) Dict.empty
        |> Grid dimension_


setAll : List ( Position, a ) -> Grid a -> Grid a
setAll list g =
    List.foldl (uncurry set) g list


mapDict : (Dict a -> Dict b) -> Grid a -> Grid b
mapDict f (Grid dimension_ dict) =
    Grid dimension_ (f dict)


set : Position -> a -> Grid a -> Grid a
set position a =
    mapDict
        (Dict.update (Position.toTuple position) (Maybe.map (\_ -> Filled a)))


slotAt : Position -> Grid a -> Maybe (Slot a)
slotAt position (Grid _ d) =
    Dict.get (Position.toTuple position) d


fill : List Position -> a -> Grid a -> Grid a
fill positions a grid =
    List.foldl (\p -> set p a) grid positions


adjacent : Position -> Grid a -> List ( Position, Slot a )
adjacent position (Grid _ d) =
    Position.adjacent position
        |> List.foldl
            (\p acc ->
                case Dict.get (Position.toTuple p) d of
                    Nothing ->
                        acc

                    Just a ->
                        ( p, a ) :: acc
            )
            []


toEntryRows : Grid a -> List (List ( Position, Slot a ))
toEntryRows (Grid dimension_ dict) =
    Dimension.toPositionRows dimension_
        |> List.map
            (List.filterMap
                (\p ->
                    Dict.get (Position.toTuple p) dict
                        |> Maybe.map (\a -> ( p, a ))
                )
            )


viewRows : (Int -> List a -> b) -> (Position -> Slot c -> a) -> Grid c -> List b
viewRows rf ef =
    toEntryRows
        >> List.indexedMap
            (\row rowEntries ->
                List.map (\( p, s ) -> ef p s) rowEntries
                    |> rf row
            )


dimension : Grid a -> Dimension
dimension (Grid dimension_ _) =
    dimension_


map : (Position -> a -> b) -> Grid a -> Grid b
map f =
    mapDict
        (Dict.map
            (\i2 slot ->
                case slot of
                    Filled a ->
                        Filled (f (Position.fromTuple i2) a)

                    Empty ->
                        Empty
            )
        )


fillWhenEmpty : List Position -> b -> Grid b -> Grid b
fillWhenEmpty positions a grid =
    List.foldl (flip setWhenEmpty a) grid positions


setWhenEmpty : Position -> b -> Grid b -> Grid b
setWhenEmpty position a =
    mapDict
        (Dict.update (Position.toTuple position)
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
