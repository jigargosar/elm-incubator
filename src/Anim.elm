module Anim exposing
    ( Anim
    , animReverse
    , animTick
    , animValue
    , initAnim
    , isDone
    , retarget
    , static
    )

import Animation as A


type Anim
    = Anim Float A.Animation


initAnim : Float -> Float -> Anim
initAnim from to =
    Anim 0
        (A.animation 0
            |> A.from from
            |> A.to to
            |> A.duration 1000
        )


animReverse : Anim -> Anim
animReverse (Anim c a) =
    A.undo c a |> Anim c


animTick : Float -> Anim -> Anim
animTick d (Anim c a) =
    Anim (c + d) a


animValue : Anim -> Float
animValue (Anim c a) =
    A.animate c a


retarget : Float -> Anim -> Anim
retarget to (Anim c a) =
    Anim c (A.retarget c to a)


static : Float -> Anim
static v =
    A.static v |> Anim 0


isDone : Anim -> Bool
isDone (Anim c a) =
    A.isDone c a
