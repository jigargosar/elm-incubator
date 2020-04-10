module Anim2 exposing (Anim, animReverse, animTick, initAnim)

import Anim
import Animation as A


type Anim
    = Anim Float A.Animation


initAnim : Float -> Float -> Anim
initAnim from to =
    Anim 0
        (A.animation 0
            |> A.from from
            |> A.to to
            |> A.duration Anim.defaultAnimDuration
        )


animReverse : Anim -> Anim
animReverse (Anim c a) =
    A.undo c a |> Anim c


animTick : Float -> Anim -> Anim
animTick d (Anim c a) =
    Anim (c + d) a
