module IncId exposing (Generator, IncId, new, newGenerator, toInt, toString)


type IncId
    = IncId Int


type Generator
    = Generator Int


newGenerator : Generator
newGenerator =
    Generator 1


new : Generator -> ( IncId, Generator )
new (Generator nextId) =
    ( IncId nextId, Generator (nextId + 1) )


toInt : IncId -> Int
toInt (IncId i) =
    i


toString : IncId -> String
toString =
    toInt >> String.fromInt
