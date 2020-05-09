module PosDict exposing (PosDict, fromLists)

import Dict exposing (Dict)
import IntPos exposing (IntPos)
import Tuple exposing (pair)


type alias PosDict a =
    Dict IntPos a


fromLists : List (List v) -> PosDict v
fromLists =
    List.indexedMap (\y -> List.indexedMap (\x -> pair ( x, y )))
        >> List.concat
        >> Dict.fromList
