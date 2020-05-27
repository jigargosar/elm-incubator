module Basics.More exposing (..)


allPass : List (a -> Bool) -> a -> Bool
allPass fs v =
    List.all ((|>) v) fs
