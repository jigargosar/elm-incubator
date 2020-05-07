module PosDict exposing (Entry, Pos, PosDict, clamp, fromRowLists)

import Dict exposing (Dict)


type alias Lists a =
    List (List a)


type alias PosDict a =
    Dict Pos a


fromRowLists : Lists a -> PosDict a
fromRowLists =
    rowListsToEntries >> Dict.fromList


type alias Size =
    { width : Int, height : Int }


clamp : Size -> PosDict a -> PosDict a
clamp size =
    Dict.filter (\pos _ -> isPosWithinSize size pos)


type alias Pos =
    ( Int, Int )


newPos : Int -> Int -> Pos
newPos x y =
    ( x, y )


isPosWithinSize : Size -> Pos -> Bool
isPosWithinSize size pos =
    let
        ( x, y ) =
            pos
    in
    x >= 0 && y >= 0 && x < size.width && y < size.height


type alias Entry a =
    ( Pos, a )


rowListsToEntries : Lists a -> List (Entry a)
rowListsToEntries =
    List.indexedMap
        (\y ->
            List.indexedMap (\x a -> ( newPos x y, a ))
        )
        >> List.concat
