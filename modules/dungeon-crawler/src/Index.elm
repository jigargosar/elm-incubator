module Index exposing (..)


range : Int -> List Int
range len =
    List.range 0 (len - 1)


range2 : Int -> Int -> List (List ( Int, Int ))
range2 l1 l2 =
    range l1
        |> List.map (\i1 -> range l2 |> List.map (Tuple.pair i1))


memberOf : Int -> Int -> Bool
memberOf len x =
    clamp 0 (len - 1) x == x
