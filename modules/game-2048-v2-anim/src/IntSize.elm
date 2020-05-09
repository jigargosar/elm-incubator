module IntSize exposing (IntSize, new, positions)

import IntPos exposing (IntPos)


type alias IntSize =
    { width : Int
    , height : Int
    }


new : Int -> Int -> IntSize
new =
    IntSize


positions : IntSize -> List IntPos
positions s =
    rangeLen s.width
        |> List.concatMap (\x -> List.map (Tuple.pair x) (rangeLen s.height))


rangeLen : Int -> List Int
rangeLen len =
    List.range 0 (len - 1)
