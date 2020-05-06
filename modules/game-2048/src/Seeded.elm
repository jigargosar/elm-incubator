module Seeded exposing (Seeded, get, init, map, maybeStep, set, view)

import Basics.Extra exposing (flip, uncurry)
import MaybeGenerator exposing (MaybeGenerator)
import Random


type Seeded a
    = Seeded Random.Seed a


init =
    Seeded


maybeStep : (a -> MaybeGenerator b) -> Seeded a -> Maybe (Seeded b)
maybeStep func (Seeded seed a) =
    MaybeGenerator.step seed (func a)
        |> Maybe.map (uncurry Seeded)


get : Seeded a -> a
get (Seeded _ a) =
    a


map : (a -> b) -> Seeded a -> Seeded b
map func model =
    get model |> func |> flip set model


set : a -> Seeded b -> Seeded a
set a (Seeded seed _) =
    Seeded seed a


view : (a -> c) -> Seeded a -> c
view func =
    get >> func
