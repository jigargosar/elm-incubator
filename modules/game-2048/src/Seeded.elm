module Seeded exposing (Seeded, get, init, maybeGenerate)

import MaybeGenerator exposing (MaybeGenerator)
import Random


type Seeded a
    = Seeded Random.Seed a


init =
    Seeded


maybeGenerate : (a -> MaybeGenerator a) -> Seeded a -> Maybe (Seeded a)
maybeGenerate func (Seeded seed a) =
    func a
        |> Maybe.map
            (\generator ->
                let
                    ( nextA, nextSeed ) =
                        Random.step generator seed
                in
                Seeded nextSeed nextA
            )


get : Seeded a -> a
get (Seeded _ a) =
    a
