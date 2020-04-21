module Grid exposing
    ( GI
    , Grid
    , entryAbove
    , entryAt
    , get
    , indices
    , init
    , map
    , set
    , swap
    , swapPair
    , toDict
    , toList
    , toListBy
    , updateAt
    , wh
    )

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


entryAt : GI -> Grid a -> Maybe ( GI, a )
entryAt i =
    get i >> Maybe.map (Tuple.pair i)


entryAbove : GI -> Grid a -> Maybe ( GI, a )
entryAbove =
    Tuple.mapSecond ((+) -1) >> entryAt


set : GI -> a -> Grid a -> Maybe (Grid a)
set gIdx a (G d) =
    if Dict.member gIdx d then
        Dict.insert gIdx a d
            |> G
            |> Just

    else
        Nothing


updateAt : GI -> (a -> a) -> Grid a -> Maybe (Grid a)
updateAt i fun g =
    get i g
        |> Maybe.andThen (fun >> flip (set i) g)


map : (GI -> a -> b) -> Grid a -> Grid b
map fun (G d) =
    Dict.map fun d |> G


indices : Grid a -> List GI
indices (G d) =
    Dict.keys d


swap : GI -> GI -> Grid a -> Maybe (Grid a)
swap ia ib grid =
    Maybe.map2 (\va vb -> set ia vb grid |> Maybe.andThen (set ib va))
        (get ia grid)
        (get ib grid)
        |> Maybe.andThen identity


swapPair : ( GI, GI ) -> Grid a -> Maybe (Grid a)
swapPair pair =
    swap (Tuple.first pair) (Tuple.second pair)


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


toDict : Grid a -> Dict GI a
toDict (G d) =
    d


toListBy : (GI -> a -> b) -> Grid a -> List b
toListBy func =
    toList >> List.map (uncurry func)


rangeLen : Int -> List Int
rangeLen len =
    List.range 0 (len - 1)


mapWH : Int -> Int -> (GI -> a) -> List a
mapWH w h func =
    rangeLen h |> List.concatMap (\y -> rangeLen w |> List.map (\x -> func ( x, y )))
