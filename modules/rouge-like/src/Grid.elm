module Grid exposing (Grid, Slot(..), adjacent, empty, fill, filled, set, setAll)

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
    = Grid (Dict a)


filled : Dimension -> a -> Grid a
filled dimension a =
    Dimension.toPositions dimension
        |> List.map Position.toTuple
        |> List.foldl (\p -> Dict.insert p (Filled a)) Dict.empty
        |> Grid


empty : Dimension -> Grid a
empty dimension =
    Dimension.toPositions dimension
        |> List.map Position.toTuple
        |> List.foldl (\p -> Dict.insert p Empty) Dict.empty
        |> Grid


setAll : List ( Position, a ) -> Grid a -> Grid a
setAll list g =
    List.foldl (uncurry set) g list


set : Position -> a -> Grid a -> Grid a
set position a (Grid d) =
    d
        |> Dict.update (Position.toTuple position) (Maybe.map (\_ -> Filled a))
        |> Grid


fill : List Position -> a -> Grid a -> Grid a
fill positions a grid =
    List.foldl (\p -> set p a) grid positions


adjacent : Position -> Grid a -> List ( Position, Slot a )
adjacent position (Grid d) =
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
