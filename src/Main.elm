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

        dr x y =
            square blue (sqW * 0.9)
                |> move (toFloat x * sqW) (toFloat y * sqW)
    in
    List.range 0 (w - 1)
        |> List.concatMap (\x -> List.range 0 (h - 1) |> List.map (dr x))


main =
    game view update init
