module Grid exposing (Grid, Slot(..), adjacent, dimension, empty, fill, filled, set, setAll, slotAt, toEntryRows, toRows)

import Basics.Extra exposing (uncurry)
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


filled : Dimension -> a -> Grid a
filled dimension_ a =
    Dimension.toPositions dimension_
        |> List.map Position.toTuple
        |> List.foldl (\p -> Dict.insert p (Filled a)) Dict.empty
        |> Grid dimension_


empty : Dimension -> Grid a
empty dimension_ =
    Dimension.toPositions dimension_
        |> List.map Position.toTuple
        |> List.foldl (\p -> Dict.insert p Empty) Dict.empty
        |> Grid dimension_


setAll : List ( Position, a ) -> Grid a -> Grid a
setAll list g =
    List.foldl (uncurry set) g list


mapDict : (Dict a -> Dict a) -> Grid a -> Grid a
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


toRows : Grid a -> List (List (Slot a))
toRows (Grid dimension_ dict) =
    Dimension.toPositionRows dimension_
        |> List.map
            (List.filterMap
                (\p ->
                    Dict.get (Position.toTuple p) dict
                )
            )


dimension : Grid a -> Dimension
dimension (Grid dimension_ _) =
    dimension_
