module Anim2 exposing (Anim2, animReverse, animTick, animValue, initAnim)

import Anim as Anim1
import Animation as A


type Anim2
    = Anim2 Float A.Animation


initAnim : Float -> Float -> Anim2
initAnim from to =
    Anim2 0
        (A.animation 0
            |> A.from from
            |> A.to to
            |> A.duration Anim1.defaultAnimDuration
        )


animReverse : Anim2 -> Anim2
animReverse (Anim2 c a) =
    A.undo c a |> Anim2 c


animTick : Float -> Anim2 -> Anim2
animTick d (Anim2 c a) =
    Anim2 (c + d) a


animValue : Anim2 -> Float
animValue (Anim2 c a) =
    A.animate c a
