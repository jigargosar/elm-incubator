module Grid exposing (Entry, Grid, Pos, PosDict, Size, fromLists, init, toDict, toRows)

import Dict exposing (Dict)
import List.Extra


type alias Pos =
    ( Int, Int )


newPos : Int -> Int -> Pos
newPos x y =
    ( x, y )


posRow : Pos -> Int
posRow ( _, y ) =
    y


type alias Entry a =
    ( Pos, a )


entryPos : Entry a -> Pos
entryPos ( pos, _ ) =
    pos


entryRow : Entry a -> Int
entryRow =
    entryPos >> posRow


type alias Size =
    { width : Int, height : Int }


type alias PosDict a =
    Dict Pos a


type Grid a
    = Grid Size (PosDict a)


init : Size -> (Pos -> a) -> Grid a
init size func =
    positionsFromSize size
        |> List.map (\pos -> ( pos, func pos ))
        |> Dict.fromList
        |> Grid size


fromLists : Size -> a -> List (List a) -> Grid a
fromLists size a lists =
    let
        posDict : PosDict a
        posDict =
            listsToPosDict lists

        func pos =
            Dict.get pos posDict |> Maybe.withDefault a
    in
    init size func


listsToPosDict : List (List a) -> PosDict a
listsToPosDict =
    List.indexedMap
        (\y ->
            List.indexedMap (\x a -> ( newPos x y, a ))
        )
        >> List.concat
        >> Dict.fromList


positionsFromSize : Size -> List Pos
positionsFromSize s =
    List.range 0 (s.width - 1)
        |> List.map
            (\x ->
                List.range 0 (s.height - 1)
                    |> List.map (newPos x)
            )
        |> List.concat


toRows : Grid a -> List (List (Entry a))
toRows (Grid _ d) =
    Dict.toList d
        |> List.Extra.gatherEqualsBy entryRow
        |> List.map consToList


toDict : Grid a -> PosDict a
toDict (Grid _ d) =
    d



-- Cons


type alias Cons a =
    ( a, List a )


consToList : Cons a -> List a
consToList ( a, xa ) =
    a :: xa
