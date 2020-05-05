module Seedy exposing (step)

import Random


step : Random.Generator a -> { b | seed : Random.Seed } -> ( a, { b | seed : Random.Seed } )
step generator model =
    let
        ( generated, seed ) =
            Random.step generator model.seed
    in
    ( generated, setSeed seed model )


setSeed : a -> { b | seed : a } -> { b | seed : a }
setSeed seed model =
    { model | seed = seed }
