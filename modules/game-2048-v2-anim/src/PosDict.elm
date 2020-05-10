module PosDict exposing (Entry, EntryList, PosDict, constrain, filled, fromLists, insertAll, insertEntry)

import Basics.Extra exposing (uncurry)
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


swap : IntPos -> IntPos -> PosDict a -> PosDict a
swap a b dict =
    dict
        |> Dict.update a (always (Dict.get b dict))
        |> Dict.update b (always (Dict.get a dict))


filled : a -> IntSize -> PosDict a
filled a s =
    IntSize.positions s
        |> List.map (pairTo a)
        |> Dict.fromList


insertEntry : Entry a -> PosDict a -> PosDict a
insertEntry =
    uncurry Dict.insert


insertAll : EntryList a -> PosDict a -> PosDict a
insertAll entryList posDict =
    List.foldl insertEntry posDict entryList


pairTo b a =
    ( a, b )


constrain : IntSize -> PosDict v -> PosDict v
constrain s =
    Dict.filter (\p _ -> IntSize.contains p s)
