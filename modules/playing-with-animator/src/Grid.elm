module Grid exposing (GI, Grid, entryAbove, get, indices, init, map, mapIdx, set, toList, toListBy, wh)

import Basics.Extra exposing (flip, uncurry)
import Dict exposing (Dict)
import List.Extra


type Grid a
    = G (IdxDict a)


type alias IdxDict a =
    Dict GI a


type alias GI =
    ( Int, Int )


init : Int -> Int -> (GI -> a) -> Grid a
init w h func =
    mapWH w h (\idx -> ( idx, func idx ))
        |> Dict.fromList
        |> G


get : GI -> Grid a -> Maybe a
get gIdx (G d) =
    Dict.get gIdx d


getEntry : GI -> Grid a -> Maybe ( GI, a )
getEntry gi =
    get gi >> Maybe.map (Tuple.pair gi)


giMove dx dy ( x, y ) =
    ( x + dx, y + dy )


giUp =
    giMove 0 -1


giDown =
    giMove 0 1


giLeft =
    giMove -1 0


giRight =
    giMove 1 0


entryAbove : GI -> Grid a -> Maybe ( GI, a )
entryAbove gi =
    getEntry (giUp gi)


set : GI -> a -> Grid a -> Maybe (Grid a)
set gIdx a (G d) =
    if Dict.member gIdx d then
        Dict.insert gIdx a d
            |> G
            |> Just

    else
        Nothing


mapIdx : GI -> (a -> a) -> Grid a -> Maybe (Grid a)
mapIdx i fun g =
    get i g
        |> Maybe.andThen (fun >> flip (set i) g)


map : (GI -> a -> b) -> Grid a -> Grid b
map fun (G d) =
    Dict.map fun d |> G


indices : Grid a -> List GI
indices (G d) =
    Dict.keys d


wh : Grid a -> ( Int, Int )
wh (G d) =
    case Dict.keys d |> List.Extra.last of
        Just ( lx, ly ) ->
            ( lx + 1, ly + 1 )

        Nothing ->
            ( 0, 0 )


toList : Grid a -> List ( GI, a )
toList (G d) =
    Dict.toList d


toListBy : (GI -> a -> b) -> Grid a -> List b
toListBy func =
    toList >> List.map (uncurry func)


rangeLen : Int -> List Int
rangeLen len =
    List.range 0 (len - 1)


mapWH : Int -> Int -> (GI -> a) -> List a
mapWH w h func =
    rangeLen h |> List.concatMap (\y -> rangeLen w |> List.map (\x -> func ( x, y )))
