module Anim1 exposing
    ( Anim
    , animReverse
    , animTick
    , animValue
    , defaultAnimDuration
    , initAnim
    )


defaultAnimDuration =
    200


type alias Anim =
    { from : Float, to : Float, duration : Float, elapsed : Float }


initAnim : Float -> Float -> Anim
initAnim from to =
    { from = from, to = to, duration = defaultAnimDuration, elapsed = 0 }


animReverse : Anim -> Anim
animReverse ({ from, to, duration, elapsed } as anim) =
    { anim | from = to, to = from, elapsed = duration - elapsed }


animProgress : Anim -> Float
animProgress { from, to, duration, elapsed } =
    if from == to || duration <= 0 || elapsed >= duration then
        1

    else
        elapsed / duration


animIsDone : Anim -> Bool
animIsDone anim =
    animProgress anim == 1


animValue : Anim -> Float
animValue ({ from, to, duration, elapsed } as anim) =
    (to - from) * animProgress anim + from


animTick : Float -> Anim -> Anim
animTick delta ({ elapsed } as anim) =
    if animIsDone anim then
        anim

    else
        { anim | elapsed = elapsed + delta }
