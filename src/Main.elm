module Main exposing (main)

import Playground exposing (..)


type Mem
    = Mem Int Int


init : Mem
init =
    Mem 13 12


update : Computer -> Mem -> Mem
update computer mem =
    mem


view : Computer -> Mem -> List Shape
view computer (Mem w h) =
    let
        sqW =
            40

        bw =
            toFloat w * sqW

        bh =
            toFloat h * sqW

        bdx =
            (sqW - bw) * 0.5

        bdy =
            (sqW - bh) * 0.5

        drc x y =
            circle blue (sqW * 0.5 * 0.9)
                |> move (toFloat x * sqW) (toFloat y * sqW)

        drb =
            List.range 0 (w - 1)
                |> List.concatMap (\x -> List.range 0 (h - 1) |> List.map (drc x))
    in
    [ group drb
        |> move bdx bdy
    ]


main =
    game view update init
