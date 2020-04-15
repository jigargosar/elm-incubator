module Grid exposing (GIdx, Grid, get, init, map, mapIdx, set, toList, toListBy, wh)

import Basics.Extra exposing (flip, uncurry)
import Dict exposing (Dict)
import List.Extra


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


mapIdx : GIdx -> (a -> a) -> Grid a -> Maybe (Grid a)
mapIdx i fun g =
    get i g
        |> Maybe.andThen (fun >> flip (set i) g)


map : (GIdx -> a -> b) -> Grid a -> Grid b
map fun (G d) =
    Dict.map fun d |> G


wh : Grid a -> ( Int, Int )
wh (G d) =
    case Dict.keys d |> List.Extra.last of
        Just ( lx, ly ) ->
            ( lx + 1, ly + 1 )

        Nothing ->
            ( 0, 0 )


toList : Grid a -> List ( GIdx, a )
toList (G d) =
    Dict.toList d


toListBy : (GIdx -> a -> b) -> Grid a -> List b
toListBy func =
    toList >> List.map (uncurry func)


rangeLen : Int -> List Int
rangeLen len =
    List.range 0 (len - 1)


mapWH : Int -> Int -> (GIdx -> a) -> List a
mapWH w h func =
    rangeLen h |> List.concatMap (\y -> rangeLen w |> List.map (\x -> func ( x, y )))
