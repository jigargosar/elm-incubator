module Grid exposing (Grid, init)

import Dict exposing (Dict)


type Grid a
    = G Int Int (IdxDict a)


type alias IdxDict a =
    Dict Idx a


type alias Idx =
    ( Int, Int )


init : Int -> Int -> (Idx -> a) -> Grid a
init w h func =
    mapWH w h (\idx -> ( idx, func idx ))
        |> Dict.fromList
        |> G w h


rangeLen : Int -> List Int
rangeLen len =
    List.range 0 (len - 1)


mapWH : Int -> Int -> (Idx -> a) -> List a
mapWH w h func =
    rangeLen h |> List.concatMap (\y -> rangeLen w |> List.map (\x -> func ( x, y )))
