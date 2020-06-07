module Main exposing (main)

import Basics.More exposing (..)
import Browser
import Browser.Events
import Dimension exposing (Dimension)
import Ease
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import HtmlStyle as HS
import Json.Decode as JD
import Location exposing (Location)
import Random exposing (Generator, Seed)
import Tuple.More as Tuple



-- Model


type alias Model =
    { dimension : Dimension
    , seed : Seed
    }


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        dimension =
            Dimension.new 12 16

        initialSeed =
            Random.initialSeed (flags.now |> always 4)
    in
    ( { dimension = dimension
      , seed = initialSeed
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | KeyDown String
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        KeyDown key ->
            ( model
            , Cmd.none
            )

        Tick delta ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown
            (JD.field "key" JD.string
                |> JD.map KeyDown
            )
        , subscribeIf False (Browser.Events.onAnimationFrameDelta Tick)
        ]



-- View


view : Model -> Html Msg
view model =
    div [ class "measure center" ]
        [ div [ class "pv3 f3" ] [ text "Dungeon Crawler" ]
        , div [ class "flex relative" ]
            [ viewGrid model
            ]
        ]



--viewOverlayMsg progress message =
--    div
--        [ class "absolute w-100 h-100 flex items-center justify-center"
--        , HS.opacity progress
--        ]
--        [ div [ class "bg-white-50 black pa3 br3" ]
--            [ div [ class "code b f2 tc" ] [ text message ]
--            , div [ class "code b f3 tc" ] [ text "Ctrl+R to restart" ]
--            ]
--        ]


type alias HM =
    Html Msg


commonStyles =
    class "w2 h2 flex items-center justify-center absolute top-0 left-0 bg-black-50"


locationToDXY location =
    Location.toTuple location
        |> Tuple.toFloatScaled 32


viewPlayerTile location hp =
    div
        [ commonStyles, HS.transforms [ HS.move (locationToDXY location) ] ]
        [ text (String.fromInt hp) ]


viewPlayerTileFading progress location hp =
    div
        [ commonStyles
        , HS.transforms [ HS.move (locationToDXY location) ]
        , HS.opacity progress
        ]
        [ text (String.fromInt hp) ]


viewPlayerTileMoving progress to location hp =
    let
        moveDXY =
            ( to, location )
                |> Tuple.map Location.toTuple
                |> uncurry Tuple.sub
                |> Tuple.toFloatScaled (32 * Ease.outQuad progress)
    in
    div
        [ commonStyles
        , HS.transforms
            [ HS.move (locationToDXY location)
            , HS.move moveDXY
            ]
        ]
        [ text (String.fromInt hp) ]


viewEnemyTile location =
    div
        [ commonStyles, HS.transforms [ HS.move (locationToDXY location) ] ]
        [ text "e" ]


viewEnemyTileDyingFromCounterAttack progress to location =
    let
        moveDXY =
            ( to, location )
                |> Tuple.map Location.toTuple
                |> uncurry Tuple.sub
                |> Tuple.toFloatScaled (32 * Ease.outQuad progress)
    in
    div
        [ commonStyles
        , HS.transforms
            [ HS.move (locationToDXY location)
            , HS.move moveDXY
            , HS.scale (Ease.reverse Ease.inOutBounce progress)
            ]
        ]
        [ text "e" ]


viewEnemyTileDying progress location =
    div
        [ commonStyles
        , HS.transforms
            [ HS.move (locationToDXY location)
            , HS.scale (Ease.reverse Ease.inOutBounce progress)
            ]
        ]
        [ text "e" ]


viewEnemyTileMoving progress to location =
    let
        moveDXY =
            ( to, location )
                |> Tuple.map Location.toTuple
                |> uncurry Tuple.sub
                |> Tuple.toFloatScaled (32 * Ease.outQuad progress)
    in
    div
        [ commonStyles
        , HS.transforms
            [ HS.move (locationToDXY location)
            , HS.move moveDXY
            ]
        ]
        [ text "e" ]


viewWallTile location =
    div
        [ commonStyles, HS.transforms [ HS.move (locationToDXY location) ] ]
        [ text "#" ]


viewFloorTile location =
    div [ commonStyles, HS.transforms [ HS.move (locationToDXY location) ] ]
        [ text "." ]


viewGrid : Model -> HM
viewGrid model =
    let
        dimension =
            model.dimension

        ( gwPx, ghPx ) =
            Dimension.toFloatScaled 32 dimension
    in
    div [ class "center code f2 bg-black white pa3 br3" ]
        [ div
            [ HS.width gwPx
            , HS.height ghPx
            , class "relative"
            ]
            ([ backgroundTileViews dimension []
             ]
                |> List.concat
            )
        ]


backgroundTileViews : Dimension -> List Location -> List HM
backgroundTileViews dimension walls =
    Dimension.toLocations dimension
        |> List.map
            (\location ->
                if List.member location walls then
                    viewWallTile location

                else
                    viewFloorTile location
            )



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
