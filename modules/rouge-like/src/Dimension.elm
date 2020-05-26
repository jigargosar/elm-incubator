module Dimension exposing (Dimension, new, toRows)

import Position exposing (Position)


type alias Dimension =
    { rows : Int
    , columns : Int
    }


new =
    Dimension


toRows : { a | rows : Int, columns : Int } -> List (List Position)
toRows d =
    rangeLen d.rows
        |> List.map
            (\r ->
                rangeLen d.columns
                    |> List.map (\c -> Position.new r c)
            )


rangeLen : Int -> List Int
rangeLen len =
    List.range 0 (len - 1)
