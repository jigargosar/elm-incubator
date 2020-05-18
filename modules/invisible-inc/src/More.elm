module More exposing (..)


neq : a -> a -> Bool
neq =
    (/=)


eq : a -> a -> Bool
eq =
    (==)


add : number -> number -> number
add =
    (+)


anyPass : List (a -> Bool) -> a -> Bool
anyPass fs x =
    List.any ((|>) x) fs


allPass : List (a -> Bool) -> a -> Bool
allPass fs x =
    List.all ((|>) x) fs
