module Main exposing (main)

import Playground exposing (..)


type Mem
    = Mem


init : Mem
init =
    Mem


update : Computer -> Mem -> Mem
update computer mem =
    mem


view : Computer -> Mem -> List Shape
view computer mem =
    []


main =
    game view update init
