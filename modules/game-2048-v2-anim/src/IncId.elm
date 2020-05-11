module IncId exposing (IncId, Seed, initialSeed, next, next2, toInt, toString)


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


next2 : Seed -> ( ( IncId, IncId ), Seed )
next2 =
    next >> Tuple.mapSecond next >> (\( a, ( b, seed ) ) -> ( ( a, b ), seed ))


toInt : IncId -> Int
toInt (IncId i) =
    i


toString : IncId -> String
toString =
    toInt >> String.fromInt
