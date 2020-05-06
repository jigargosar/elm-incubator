module Seeded exposing (Seeded, get, init, maybeStep)

import Basics.Extra exposing (uncurry)
import MaybeGenerator exposing (MaybeGenerator)
import Random


type Seeded a
    = Seeded Random.Seed a


init =
    Seeded


maybeStep : (a -> MaybeGenerator a) -> Seeded a -> Maybe (Seeded a)
maybeStep func (Seeded seed a) =
    MaybeGenerator.step seed (func a)
        |> Maybe.map (uncurry Seeded)


get : Seeded a -> a
get (Seeded _ a) =
    a
