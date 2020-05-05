module MaybeGenerator exposing (MaybeGenerator, map)

import Random


type alias MaybeGenerator a =
    Maybe (Random.Generator a)


map func =
    Maybe.map (Random.map func)
