module Index exposing (..)


range : Int -> List Int
range len =
    List.range 0 (len - 1)


memberOf : Int -> Int -> Bool
memberOf len x =
    clamp 0 (len - 1) x == x
