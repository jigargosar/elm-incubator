module Main exposing (main)

import Playground exposing (..)


type alias Mem =
    {}


init : Mem
init =
    {}


update : Computer -> Mem -> Mem
update computer mem =
    mem


view : Computer -> Mem -> List Shape
view computer mem =
    []


main =
    game view update init
