module Grid exposing (Entry, Grid, Lists, Pos, PosDict, Size, fromLists, get, init, set, toDict, toLists, transpose)

import Basics.Extra exposing (uncurry)
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


entryValue : Entry a -> a
entryValue ( _, a ) =
    a


entryRow : Entry a -> Int
entryRow =
    entryPos >> posRow


type alias Size =
    { width : Int, height : Int }


transposeSize : Size -> Size
transposeSize size =
    { width = size.height, height = size.width }


type alias Lists a =
    List (List a)


type alias PosDict a =
    Dict Pos a


posDictToRowLists : PosDict a -> Lists a
posDictToRowLists d =
    Dict.toList d
        |> List.Extra.gatherEqualsBy entryRow
        |> List.map consToList
        |> List.map (List.map entryValue)


type Grid a
    = Grid Size (PosDict a)


init : Size -> (Pos -> a) -> Grid a
init size func =
    positionsFromSize size
        |> List.map (\pos -> ( pos, func pos ))
        |> Dict.fromList
        |> Grid size


fromLists : Size -> a -> Lists a -> Grid a
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



--noinspection ElmUnusedSymbol


get : Pos -> Grid a -> Maybe a
get pos (Grid _ d) =
    Dict.get pos d


listsToPosDict : Lists a -> PosDict a
listsToPosDict =
    listsToEntries >> Dict.fromList


listsToEntries : Lists a -> List (Entry a)
listsToEntries =
    List.indexedMap
        (\y ->
            List.indexedMap (\x a -> ( newPos x y, a ))
        )
        >> List.concat


positionsFromSize : Size -> List Pos
positionsFromSize s =
    List.range 0 (s.width - 1)
        |> List.map
            (\x ->
                List.range 0 (s.height - 1)
                    |> List.map (newPos x)
            )
        |> List.concat


toLists : Grid a -> Lists a
toLists (Grid _ d) =
    Dict.toList d
        |> List.Extra.gatherEqualsBy entryRow
        |> List.map consToList
        |> List.map (List.map entryValue)


mapRowLists : (List a -> List a) -> Grid a -> Grid a
mapRowLists func (Grid s d) =
    let
        newEntries : List (Entry a)
        newEntries =
            Dict.toList d
                |> List.Extra.gatherEqualsBy entryRow
                |> List.map consToList
                |> List.map (List.map entryValue)
                |> List.map func
                |> listsToEntries
    in
    replaceEntries newEntries d
        |> Grid s


replaceEntries : List ( comparable, b ) -> Dict comparable b -> Dict comparable b
replaceEntries entries dict =
    List.foldl (uncurry replace) dict entries


replace : comparable -> b -> Dict comparable b -> Dict comparable b
replace k v =
    Dict.update k (Maybe.map (always v))


toDict : Grid a -> PosDict a
toDict (Grid _ d) =
    d



-- Cons


type alias Cons a =
    ( a, List a )


consToList : Cons a -> List a
consToList ( a, xa ) =
    a :: xa
