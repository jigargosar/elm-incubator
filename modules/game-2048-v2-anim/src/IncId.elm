module IncId exposing (Generator, IncId, new, newGenerator)


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
