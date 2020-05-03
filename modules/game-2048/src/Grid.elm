module Grid exposing (Grid, Pos, Size, init)

import Dict exposing (Dict)


type alias Pos =
    ( Int, Int )


newPos : Int -> Int -> Pos
newPos x y =
    ( x, y )


type alias Size =
    { width : Int, height : Int }


type alias PosDict a =
    Dict Pos a


type Grid a
    = Grid Size (PosDict a)


init : Size -> (Pos -> a) -> Grid a
init size func =
    positionsFromSize size
        |> List.map (\pos -> ( pos, func pos ))
        |> Dict.fromList
        |> Grid size


positionsFromSize : Size -> List Pos
positionsFromSize size =
    List.range 0 (size.width - 1)
        |> List.map
            (\x ->
                List.range 0 (size.height - 1)
                    |> List.map (newPos x)
            )
        |> List.concat
