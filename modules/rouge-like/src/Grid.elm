module Grid exposing (Grid, Slot(..), empty, fill, set)

import Dict
import Dimension exposing (Dimension)
import Position exposing (Position)


type alias Dict a =
    Dict.Dict ( Int, Int ) (Slot a)


type Slot a
    = Empty
    | Filled a


type Grid a
    = Grid (Dict a)


empty : Dimension -> Grid a
empty dimension =
    Dimension.toPositions dimension
        |> List.map Position.toTuple
        |> List.foldl (\p -> Dict.insert p Empty) Dict.empty
        |> Grid


set : Position -> a -> Grid a -> Grid a
set position a (Grid d) =
    d
        |> Dict.update (Position.toTuple position) (Maybe.map (\_ -> Filled a))
        |> Grid


fill : List Position -> a -> Grid a -> Grid a
fill positions a grid =
    List.foldl (\p -> set p a) grid positions
