module PointerEvents exposing
    ( onDown
    , onEnter
    , onPrimaryDown
    , onPrimaryEnterAndDown
    )

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as DX


onEnter =
    on "pointerenter"


onDown =
    on "pointerdown"


onPrimaryDown : a -> Attribute a
onPrimaryDown msg =
    onDown (succeedWhenPrimaryDown msg)


onPrimaryEnterAndDown : a -> Attribute a
onPrimaryEnterAndDown msg =
    onEnter (succeedWhenPrimaryDown msg)


type alias PE =
    { pointerId : Int
    , pointerType : String
    , type_ : String
    , button : Int
    , buttons : Int
    , isPrimary : Bool
    , pressure : Float
    , target : El
    , offsetX : Float
    , offsetY : Float
    , pageX : Float
    , pageY : Float
    }


type alias El =
    { offsetLeft : Float
    , offsetTop : Float
    , offsetWidth : Float
    , offsetHeight : Float
    }


elDecoder =
    D.map4 El
        (D.field "offsetLeft" D.float)
        (D.field "offsetTop" D.float)
        (D.field "offsetWidth" D.float)
        (D.field "offsetHeight" D.float)


andMap =
    D.map2 (|>)


peDecoder : Decoder PE
peDecoder =
    D.succeed PE
        |> andMap (D.field "pointerId" D.int)
        |> andMap (D.field "pointerType" D.string)
        |> andMap (D.field "type" D.string)
        |> andMap (D.field "button" D.int)
        |> andMap (D.field "buttons" D.int)
        |> andMap (D.field "isPrimary" D.bool)
        |> andMap (D.field "pressure" D.float)
        |> andMap (D.field "target" elDecoder)
        |> andMap (D.field "offsetX" D.float)
        |> andMap (D.field "offsetY" D.float)
        |> andMap (D.field "pageX" D.float)
        --|> andThenTapLog "PE"
        |> andMap (D.field "pageY" D.float)


isPrimaryDown : PE -> Bool
isPrimaryDown pe =
    pe.buttons == 1



--noinspection ElmUnusedSymbol


andThenTapLog logMsg =
    D.andThen (\v -> D.succeed (Debug.log logMsg v))



--noinspection ElmUnusedSymbol


andThenDebugFail =
    D.andThen (Debug.log "debug" >> (\_ -> D.fail ""))


succeedWhenPrimaryDown : a -> D.Decoder a
succeedWhenPrimaryDown msg =
    DX.when peDecoder isPrimaryDown (D.succeed msg)
