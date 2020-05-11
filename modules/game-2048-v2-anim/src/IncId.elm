module IncId exposing (IncId, Seed, initialSeed, next, toInt, toString)


type IncId
    = IncId Int


type Seed
    = Seed Int


initialSeed : Seed
initialSeed =
    Seed 1


next : Seed -> ( IncId, Seed )
next (Seed nextId) =
    ( IncId nextId, Seed (nextId + 1) )


toInt : IncId -> Int
toInt (IncId i) =
    i


toString : IncId -> String
toString =
    toInt >> String.fromInt
