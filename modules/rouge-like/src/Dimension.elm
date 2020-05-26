module Dimension exposing (Dimension, member, new, toRows)

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


memberLen : Int -> Int -> Bool
memberLen len x =
    clamp 0 (len - 1) x == x


member : Position -> Dimension -> Bool
member position dimension =
    case Position.toTuple position of
        ( row, column ) ->
            (clamp 0 (dimension.rows - 1) row == row)
                && (clamp 0 (dimension.columns - 1) column == column)
