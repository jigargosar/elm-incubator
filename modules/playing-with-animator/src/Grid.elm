module Grid exposing (GIdx, Grid, get, init, set)

import Dict exposing (Dict)


type Grid a
    = G (IdxDict a)


type alias IdxDict a =
    Dict GIdx a


type alias GIdx =
    ( Int, Int )


init : Int -> Int -> (GIdx -> a) -> Grid a
init w h func =
    mapWH w h (\idx -> ( idx, func idx ))
        |> Dict.fromList
        |> G


get : GIdx -> Grid a -> Maybe a
get gIdx (G d) =
    Dict.get gIdx d


set : GIdx -> a -> Grid a -> Maybe (Grid a)
set gIdx a (G d) =
    if Dict.member gIdx d then
        Dict.insert gIdx a d
            |> G
            |> Just

    else
        Nothing


rangeLen : Int -> List Int
rangeLen len =
    List.range 0 (len - 1)


mapWH : Int -> Int -> (GIdx -> a) -> List a
mapWH w h func =
    rangeLen h |> List.concatMap (\y -> rangeLen w |> List.map (\x -> func ( x, y )))
