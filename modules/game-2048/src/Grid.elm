module Grid exposing (Entry, Grid, Pos, PosDict, Size, fromLists, get, init, positions, set, toDict, toRows, transpose)

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


transposePos : Pos -> Pos
transposePos ( x, y ) =
    newPos y x


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


transposeSize : Size -> Size
transposeSize size =
    { width = size.height, height = size.width }


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


transpose : Grid a -> Grid a
transpose ((Grid s d) as grid) =
    case Dict.get (newPos 0 0) d of
        Nothing ->
            grid

        Just defaultValue ->
            let
                func pos =
                    Dict.get (transposePos pos) d |> Maybe.withDefault defaultValue
            in
            init (transposeSize s) func


set : Pos -> a -> Grid a -> Maybe (Grid a)
set pos a (Grid s d) =
    Dict.get pos d
        |> Maybe.map (\_ -> Dict.insert pos a d |> Grid s)


get : Pos -> Grid a -> Maybe a
get pos (Grid _ d) =
    Dict.get pos d


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


positions : Grid a -> List Pos
positions (Grid _ d) =
    Dict.keys d


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
