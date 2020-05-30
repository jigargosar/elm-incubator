module RCGrid exposing (Grid, Slot(..), adjacent, empty, fill, filled, set, setAll, toEntryRows)

import Basics.Extra exposing (uncurry)
import Dict
import Dimension exposing (Dimension)
import Position exposing (Position)
import RC exposing (RC)


type alias Dict a =
    Dict.Dict RC (Slot a)


type Slot a
    = Empty
    | Filled a


type Grid a
    = Grid Dimension (Dict a)


filled : Dimension -> a -> Grid a
filled dimension a =
    Dimension.toRCRows dimension
        |> List.concat
        |> List.foldl (\rc -> Dict.insert rc (Filled a)) Dict.empty
        |> Grid dimension


empty : Dimension -> Grid a
empty dimension =
    Dimension.toPositions dimension
        |> List.map Position.toTuple
        |> List.foldl (\p -> Dict.insert p Empty) Dict.empty
        |> Grid dimension


setAll : List ( Position, a ) -> Grid a -> Grid a
setAll list g =
    List.foldl (uncurry set) g list


mapDict : (Dict a -> Dict a) -> Grid a -> Grid a
mapDict f (Grid dimension dict) =
    Grid dimension (f dict)


set : Position -> a -> Grid a -> Grid a
set position a =
    mapDict
        (Dict.update (Position.toTuple position) (Maybe.map (\_ -> Filled a)))


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
toEntryRows (Grid dimension dict) =
    Dimension.toPositionRows dimension
        |> List.map
            (List.filterMap
                (\p ->
                    Dict.get (Position.toTuple p) dict
                        |> Maybe.map (\a -> ( p, a ))
                )
            )
