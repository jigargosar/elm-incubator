module PosDict exposing (Entry, EntryList, PosDict, clamp, fill, fromLists)

import Dict exposing (Dict)
import IntPos exposing (IntPos)
import IntSize exposing (IntSize)
import Tuple exposing (pair)


type alias PosDict a =
    Dict IntPos a


type alias Entry a =
    ( IntPos, a )


type alias EntryList a =
    List (Entry a)


fromLists : List (List v) -> PosDict v
fromLists =
    List.indexedMap (\y -> List.indexedMap (\x -> pair ( x, y )))
        >> List.concat
        >> Dict.fromList


fill : a -> IntSize -> PosDict a
fill a s =
    IntSize.positions s
        |> List.map (pairTo a)
        |> Dict.fromList


pairTo b a =
    ( a, b )


clamp : IntSize -> PosDict v -> PosDict v
clamp s =
    Dict.filter (\p _ -> IntSize.contains p s)
