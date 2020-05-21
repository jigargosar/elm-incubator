module IntSize exposing (IntPos, IntSize, member, positions)

import More exposing (..)


type alias IntSize =
    { width : Int
    , height : Int
    }


type alias IntPos =
    ( Int, Int )


member : IntPos -> IntSize -> Bool
member ( x, y ) s =
    (clamp 0 (s.width - 1) x == x)
        && (clamp 0 (s.height - 1) y == y)


positions : IntSize -> List IntPos
positions s =
    List.range 0 (s.width - 1)
        |> List.concatMap (\x -> List.range 0 (s.height - 1) |> List.map (pair x))


adjacentOf pos =
    [ mapFirst (add 1)
    , mapFirst (add -1)
    , mapSecond (add 1)
    , mapSecond (add -1)
    ]
        |> List.map (applyTo pos)
