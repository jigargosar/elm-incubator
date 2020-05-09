module PosDict exposing (PosDict, clamp, fromLists)

import Dict exposing (Dict)
import IntPos exposing (IntPos)
import IntSize exposing (IntSize)
import Tuple exposing (pair)


type alias PosDict a =
    Dict IntPos a


fromLists : List (List v) -> PosDict v
fromLists =
    List.indexedMap (\y -> List.indexedMap (\x -> pair ( x, y )))
        >> List.concat
        >> Dict.fromList


clamp : IntSize -> PosDict v -> PosDict v
clamp s =
    Dict.filter (\p _ -> IntSize.contains p s)
