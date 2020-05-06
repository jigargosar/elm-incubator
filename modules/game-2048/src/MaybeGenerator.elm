module MaybeGenerator exposing (MaybeGenerator, map, step)

import Basics.Extra exposing (flip, swap)
import Random


type alias MaybeGenerator a =
    Maybe (Random.Generator a)


map func =
    Maybe.map (Random.map func)


step : Random.Seed -> MaybeGenerator a -> Maybe ( Random.Seed, a )
step seed =
    Maybe.map (flip Random.step seed >> swap)
