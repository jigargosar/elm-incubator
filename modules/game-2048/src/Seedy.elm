module Seedy exposing (generate, maybeGenerate)

import MaybeGenerator exposing (MaybeGenerator)
import Random


generate : Random.Generator a -> { b | seed : Random.Seed } -> ( a, { b | seed : Random.Seed } )
generate generator model =
    let
        ( generated, seed ) =
            Random.step generator model.seed
    in
    ( generated, setSeed seed model )


maybeGenerate : MaybeGenerator a -> { b | seed : Random.Seed } -> Maybe ( a, { b | seed : Random.Seed } )
maybeGenerate mg model =
    Maybe.map (\g -> generate g model) mg


setSeed : a -> { b | seed : a } -> { b | seed : a }
setSeed seed model =
    { model | seed = seed }
