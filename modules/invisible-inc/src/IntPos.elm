module IntPos exposing (IntPos, adjacent)

import More exposing (..)


type alias IntPos =
    ( Int, Int )


adjacent : IntPos -> List IntPos
adjacent pos =
    [ mapFirst (add 1)
    , mapFirst (add -1)
    , mapSecond (add 1)
    , mapSecond (add -1)
    ]
        |> List.map (applyTo pos)
