module Grid exposing
    ( Entry
    , Grid
    , Lists
    , Pos
    , PosDict
    , Size
    , fromRowLists
    , get
    , init
    , map
    , mapColumnLists
    , mapRowLists
    , replaceEntries
    , replaceFromDict
    , reverseColumns
    , reverseRows
    , set
    , setEntry
    , toColumnEntries
    , toDict
    , toLists
    , toRowEntries
    )

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


posColumn : Pos -> Int
posColumn ( x, _ ) =
    x


transposePos : Pos -> Pos
transposePos ( x, y ) =
    newPos y x


type alias Entry a =
    ( Pos, a )


entryPos : Entry a -> Pos
entryPos ( pos, _ ) =
    pos


getEntryValue : Entry a -> a
getEntryValue ( _, a ) =
    a


getEntryRowIndex : Entry a -> Int
getEntryRowIndex =
    entryPos >> posRow


getEntryColumnIndex : Entry a -> Int
getEntryColumnIndex =
    entryPos >> posColumn


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
        |> List.Extra.gatherEqualsBy getEntryRowIndex
        |> List.map (consToList >> List.map getEntryValue)


posDictToColumnLists : PosDict a -> Lists a
posDictToColumnLists d =
    Dict.toList d
        |> List.Extra.gatherEqualsBy getEntryColumnIndex
        |> List.map (consToList >> List.map getEntryValue)


type Grid a
    = Grid Size (PosDict a)


init : Size -> (Pos -> a) -> Grid a
init size func =
    positionsFromSize size
        |> List.map (\pos -> ( pos, func pos ))
        |> Dict.fromList
        |> Grid size


fromRowLists : Size -> a -> Lists a -> Grid a
fromRowLists size a lists =
    let
        posDict : PosDict a
        posDict =
            listsToPosDict lists

        func pos =
            Dict.get pos posDict |> Maybe.withDefault a
    in
    init size func


set : Pos -> a -> Grid a -> Maybe (Grid a)
set pos a (Grid s d) =
    Dict.get pos d
        |> Maybe.map (\_ -> Dict.insert pos a d |> Grid s)


setEntry : Entry a -> Grid a -> Maybe (Grid a)
setEntry ( pos, a ) =
    set pos a


get : Pos -> Grid a -> Maybe a
get pos (Grid _ d) =
    Dict.get pos d


replaceFromDict : PosDict a -> Grid a -> Grid a
replaceFromDict posDict (Grid s d) =
    dictReplaceEntries (Dict.toList posDict) d
        |> Grid s


replaceEntries : List (Entry a) -> Grid a -> Grid a
replaceEntries entries (Grid s d) =
    dictReplaceEntries entries d |> Grid s


map : (Pos -> a -> b) -> Grid a -> Grid b
map func (Grid s d) =
    Dict.map func d |> Grid s


listsToPosDict : Lists a -> PosDict a
listsToPosDict =
    rowListsToEntries >> Dict.fromList


rowListsToEntries : Lists a -> List (Entry a)
rowListsToEntries =
    List.indexedMap
        (\y ->
            List.indexedMap (\x a -> ( newPos x y, a ))
        )
        >> List.concat


columnListsToEntries : Lists a -> List (Entry a)
columnListsToEntries =
    List.indexedMap
        (\x ->
            List.indexedMap (\y a -> ( newPos x y, a ))
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
        |> List.Extra.gatherEqualsBy getEntryRowIndex
        |> List.map consToList
        |> List.map (List.map getEntryValue)


reverseRows : Grid a -> Grid a
reverseRows (Grid s d) =
    let
        reverseXPos ( x, y ) =
            ( s.width - 1 - x, y )

        func pos v =
            Dict.get (reverseXPos pos) d |> Maybe.withDefault v
    in
    Grid s (Dict.map func d)


reverseColumns : Grid a -> Grid a
reverseColumns (Grid s d) =
    let
        reverseYPos ( x, y ) =
            ( x, s.height - 1 - y )

        func pos v =
            Dict.get (reverseYPos pos) d |> Maybe.withDefault v
    in
    Grid s (Dict.map func d)


mapRowLists : (List a -> List a) -> Grid a -> Grid a
mapRowLists func (Grid s d) =
    let
        newEntries : List (Entry a)
        newEntries =
            d
                |> posDictToRowLists
                |> List.map func
                |> rowListsToEntries
    in
    dictReplaceEntries newEntries d
        |> Grid s


mapColumnLists : (List a -> List a) -> Grid a -> Grid a
mapColumnLists func (Grid s d) =
    let
        newEntries : List (Entry a)
        newEntries =
            d
                |> posDictToColumnLists
                |> List.map func
                |> columnListsToEntries
    in
    dictReplaceEntries newEntries d
        |> Grid s


toDict : Grid a -> PosDict a
toDict (Grid _ d) =
    d


toRowEntries : Grid a -> List (List (Entry a))
toRowEntries (Grid _ d) =
    Dict.toList d
        |> List.Extra.gatherEqualsBy getEntryRowIndex
        |> List.map consToList


toColumnEntries : Grid a -> List (List (Entry a))
toColumnEntries (Grid _ d) =
    Dict.toList d
        |> List.Extra.gatherEqualsBy getEntryColumnIndex
        |> List.map consToList



-- Cons


type alias Cons a =
    ( a, List a )


consToList : Cons a -> List a
consToList ( a, xa ) =
    a :: xa



-- Dict Helpers


dictReplaceEntries : List ( comparable, b ) -> Dict comparable b -> Dict comparable b
dictReplaceEntries entries dict =
    List.foldl (uncurry dictReplace) dict entries


dictReplace : comparable -> b -> Dict comparable b -> Dict comparable b
dictReplace k v =
    Dict.update k (Maybe.map (always v))
