module Main exposing (main)

import Playground exposing (..)


type Mem
    = Mem Int Int


init : Mem
init =
    Mem 8 10


update : Computer -> Mem -> Mem
update computer mem =
    mem


view : Computer -> Mem -> List Shape
view computer (Mem w h) =
    let
        sqW =
            50

        bw =
            toFloat w * sqW

        bh =
            toFloat h * sqW

        dCell x y =
            group
                [ square lightGreen sqW |> fade 0
                , circle blue (sqW * 0.2) |> fade 1
                ]
                |> move (toFloat x * sqW) (toFloat y * sqW)

        dBoardCells =
            List.range 0 (w - 1)
                |> List.concatMap (\x -> List.range 0 (h - 1) |> List.map (dCell x))
    in
    [ rectangle yellow (bw + sqW) (bh + sqW) |> fade 0.2
    , group dBoardCells
        |> move ((sqW - bw) * 0.5) ((sqW - bh) * 0.5)
    ]


main =
    game view update init
