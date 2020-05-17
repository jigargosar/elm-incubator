port module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as JE exposing (Value)
import List.Extra as List
import ListZipper as LZ exposing (ListZipper)
import Process
import Set exposing (Set)
import String exposing (fromInt)
import Task
import Tuple exposing (..)


port cache : String -> Cmd msg


port gotBeacons : (Value -> msg) -> Sub msg


port getBeacons : () -> Cmd msg



-- Path


type alias Path =
    List IntPos



-- Guard


type alias Guard =
    { path : ListZipper IntPos
    }


initGuard : Guard
initGuard =
    let
        startPos =
            ( 8, 12 )

        path =
            List.range 0 5
                |> List.scanl (\_ -> mapFirst (add -1)) startPos
                |> LZ.fromList
                |> Maybe.withDefault (LZ.singleton startPos)
    in
    { path = path
    }


stepGuard : Guard -> ( Bool, Guard )
stepGuard guard =
    case LZ.right guard.path of
        Just path ->
            ( False, { guard | path = path } )

        Nothing ->
            ( True, { guard | path = LZ.swap guard.path } )


add =
    (+)



-- Model


type alias Model =
    { guard : Guard
    , status : Status
    , wallPositions : Set IntPos
    }


type Status
    = PlayerTurn
    | EnemyTurn


type alias Flags =
    ()


initialWallPositions =
    [ hWall 5 ( 2, 8 )
    , vWall 6 ( 7, 6 )
    , vWall 8 ( 1, 4 )
    ]
        |> List.concat


hWall n start =
    List.range 0 (n - 1)
        |> List.scanl (always (mapFirst (add 1))) start


vWall n start =
    List.range 0 (n - 1)
        |> List.scanl (always (mapSecond (add 1))) start


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { guard = initGuard
      , status = PlayerTurn
      , wallPositions = Set.fromList initialWallPositions
      }
    , Cmd.none
    )


triggerStepEnemyTurn =
    Process.sleep 100 |> Task.perform (always StepEnemyTurn)



-- Update


type Msg
    = NoOp
    | StepEnemyTurn
    | EndTurnClicked
    | GotBeacons Value


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        StepEnemyTurn ->
            let
                ( isDone, guard ) =
                    stepGuard model.guard
            in
            ( { model
                | guard = guard
                , status =
                    if isDone then
                        PlayerTurn

                    else
                        model.status
              }
            , if isDone then
                Cmd.none

              else
                triggerStepEnemyTurn
            )

        EndTurnClicked ->
            case model.status of
                PlayerTurn ->
                    ( { model | status = EnemyTurn }, triggerStepEnemyTurn )

                EnemyTurn ->
                    ( model, Cmd.none )

        GotBeacons encoded ->
            case JD.decodeValue (JD.list beaconDecoder) encoded of
                Err error ->
                    let
                        _ =
                            Debug.log "error" error
                    in
                    ( model, Cmd.none )

                Ok beacons ->
                    let
                        _ =
                            Debug.log "beacons" beacons
                    in
                    ( model, Cmd.none )


type alias Beacon =
    { pos : IntPos
    , rect : Rect
    }


type alias Rect =
    { x : Float, y : Float, width : Float, height : Float }


beaconDecoder : Decoder Beacon
beaconDecoder =
    JD.succeed Beacon
        |> required "pos" (JD.map2 pair (JD.index 0 JD.int) (JD.index 1 JD.int))
        |> required "rect" rectDecoder


rectDecoder : Decoder Rect
rectDecoder =
    JD.succeed Rect
        |> required "x" JD.float
        |> required "y" JD.float
        |> required "width" JD.float
        |> required "height" JD.float


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ gotBeacons GotBeacons ]



-- View


type alias DM =
    Document Msg


type alias HM =
    Html Msg


view : Model -> DM
view model =
    Document "Invisible Inc."
        [ div
            [ class "center"
            , style "width" (fromInt gridWidthPx ++ "px")
            ]
            [ div [ class "pv2 flex items-center " ]
                [ div [ class "mr3 flex-auto f3" ] [ text (Debug.toString model.status) ]
                , div [ class "" ] [ endTurnButton ]
                ]
            , viewGrid model
            ]
        ]


endTurnButton =
    button
        [ class "ma0 bn"
        , class "pv2 ph3 br3 f4"
        , style "background-color" "hsl(209, 100%, 79%)"
        , style "color" "hsl(209, 100%, 20%)"
        , onClick EndTurnClicked
        ]
        [ text "End Turn" ]


gridSize =
    { width = 10
    , height = 15
    }


cellWidthPx =
    50


gridWidthPx =
    cellWidthPx * gridSize.width


gridHeightPx =
    cellWidthPx * gridSize.height


viewGrid : Model -> HM
viewGrid model =
    div
        [ style "width" (fromInt gridWidthPx ++ "px")
        , style "height" (fromInt gridHeightPx ++ "px")
        ]
        ((positionsOf gridSize
            |> List.map viewBackgroundTile
         )
            ++ (model.wallPositions |> Set.toList |> List.map viewWall)
            ++ [ viewPlayer, viewGuard model.guard ]
        )


viewPlayer =
    let
        pos =
            ( 5, 5 )
    in
    div
        [ style "width" (fromInt cellWidthPx ++ "px")
        , style "height" (fromInt cellWidthPx ++ "px")
        , style "transform" (renderCellTransform pos)
        , style "padding" "3px"
        , class "absolute"
        ]
        [ div [ class "w-100 h-100 br3 bg-light-blue" ] [] ]


viewGuard : Guard -> HM
viewGuard guard =
    let
        pos =
            LZ.current guard.path
    in
    div
        [ style "width" (fromInt cellWidthPx ++ "px")
        , style "height" (fromInt cellWidthPx ++ "px")
        , style "transform" (renderCellTransform pos)
        , style "padding" "3px"
        , class "absolute"
        ]
        [ div [ class "w-100 h-100 br3 bg-pink" ] [] ]


viewWall : IntPos -> HM
viewWall pos =
    div
        [ style "width" (fromInt cellWidthPx ++ "px")
        , style "height" (fromInt cellWidthPx ++ "px")
        , style "transform" (renderCellTransform pos)
        , style "padding" "3px"
        , class "absolute"
        ]
        [ div
            [ class "w-100 h-100 br3 o-70"
            , style "background-color" "hsl(0, 25%, 65%)"
            ]
            []
        ]


viewBackgroundTile pos =
    div
        [ style "width" (fromInt cellWidthPx ++ "px")
        , style "height" (fromInt cellWidthPx ++ "px")
        , style "transform" (renderCellTransform pos)
        , style "padding" "3px"
        , class "absolute"
        , Html.Attributes.attribute "data-beacon" (intPosEncoder pos |> JE.encode 0)
        ]
        [ div [ class "w-100 h-100 br3 ba bw1 b--light-blue" ] [] ]


renderCellTransform ( x, y ) =
    "translate(" ++ String.fromInt (x * cellWidthPx) ++ "px, " ++ String.fromInt (y * cellWidthPx) ++ "px)"


type alias IntPos =
    ( Int, Int )


intPosEncoder : IntPos -> Value
intPosEncoder intPos =
    (\( a, b ) -> JE.list identity [ JE.int a, JE.int b ]) intPos


type alias IntSize =
    { width : Int
    , height : Int
    }


positionsOf : IntSize -> List IntPos
positionsOf s =
    List.range 0 (s.width - 1)
        |> List.concatMap (\x -> List.range 0 (s.height - 1) |> List.map (pair x))



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
