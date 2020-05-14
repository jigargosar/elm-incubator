module IncId exposing
    ( IdDict
    , IncId
    , Seed
    , dictFromListBy
    , dictGet
    , dictInsert
    , dictValues
    , emptyDict
    , initialSeed
    , next
    , seedDecoder
    , seedEncoder
    , toInt
    , toString
    )

import Dict
import Dict.Extra as Dict
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


type IncId
    = IncId Int


type Seed
    = Seed Int


initialSeed : Seed
initialSeed =
    Seed 1


seedEncoder : Seed -> Value
seedEncoder (Seed nextId) =
    JE.object
        [ ( "tag", JE.string "IncId.Seed" )
        , ( "nextId", JE.int nextId )
        ]


seedDecoder : Decoder Seed
seedDecoder =
    JD.map Seed JD.int


next : Seed -> ( IncId, Seed )
next (Seed nextId) =
    ( IncId nextId, Seed (nextId + 1) )


toInt : IncId -> Int
toInt (IncId i) =
    i


toString : IncId -> String
toString =
    toInt >> String.fromInt


type IdDict a
    = IdDict (Dict.Dict Int a)


emptyDict : IdDict a
emptyDict =
    IdDict Dict.empty


dictFromListBy : (a -> IncId) -> List a -> IdDict a
dictFromListBy idFunc =
    Dict.fromListBy (idFunc >> toInt)
        >> IdDict


dictValues : IdDict a -> List a
dictValues (IdDict d) =
    Dict.values d


dictGet : IncId -> IdDict a -> Maybe a
dictGet id (IdDict d) =
    Dict.get (toInt id) d


dictInsert : IncId -> a -> IdDict a -> IdDict a
dictInsert id a (IdDict d) =
    Dict.insert (toInt id) a d
        |> IdDict
