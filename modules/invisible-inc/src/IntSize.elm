module IntSize exposing (IntSize, adjacentMembers, member, memberOf, positions)

import IntPos exposing (IntPos)
import More exposing (..)


type alias IntSize =
    { width : Int
    , height : Int
    }


member : IntPos -> IntSize -> Bool
member ( x, y ) s =
    (clamp 0 (s.width - 1) x == x)
        && (clamp 0 (s.height - 1) y == y)


memberOf : IntSize -> IntPos -> Bool
memberOf =
    flip member


positions : IntSize -> List IntPos
positions s =
    List.range 0 (s.width - 1)
        |> List.concatMap (\x -> List.range 0 (s.height - 1) |> List.map (pair x))


adjacentMembers : IntPos -> IntSize -> List IntPos
adjacentMembers pos s =
    IntPos.adjacent pos
        |> List.filter (memberOf s)
