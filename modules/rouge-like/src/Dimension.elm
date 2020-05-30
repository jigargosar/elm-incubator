module Dimension exposing
    ( Dimension
    , maxPosition
    , member
    , new
    , toPositions
    , toRows
    )

import Index
import Position exposing (Position)


type alias Dimension =
    { rows : Int
    , columns : Int
    }


new =
    Dimension


toRows : Dimension -> List (List Position)
toRows d =
    Index.range d.rows
        |> List.map
            (\r ->
                Index.range d.columns
                    |> List.map (\c -> Position.new r c)
            )


toPositions : Dimension -> List Position
toPositions =
    toRows >> List.concat


member : Position -> Dimension -> Bool
member position dimension =
    let
        ( row, column ) =
            Position.toTuple position
    in
    Index.memberOf dimension.rows row
        && Index.memberOf dimension.columns column


maxPosition : Dimension -> Position
maxPosition dimension =
    Position.new (dimension.rows - 1) (dimension.columns - 1)
