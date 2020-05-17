module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (onClick)
import Process
import String exposing (fromInt)
import Task
import Tuple exposing (..)



-- Guard


type alias Guard =
    { pos : IntPos
    }


initGuard : Guard
initGuard =
    { pos = ( 8, 12 ) }


moveGuard : Guard -> Guard
moveGuard guard =
    { guard | pos = mapFirst (add -1) guard.pos }


add =
    (+)



-- Model


type alias Model =
    { guard : Guard
    , status : Status
    }


type Status
    = PlayerTurn
    | EnemyTurn


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { guard = initGuard
      , status = PlayerTurn
      }
    , Cmd.none
    )


triggerStepEnemyTurn =
    Process.sleep 1000 |> Task.perform (always StepEnemyTurn)



-- Update


type Msg
    = NoOp
    | StepEnemyTurn
    | EndTurnClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        StepEnemyTurn ->
            ( { model | guard = moveGuard model.guard }, Cmd.none )

        EndTurnClicked ->
            case model.status of
                PlayerTurn ->
                    ( { model | status = EnemyTurn }, triggerStepEnemyTurn )

                EnemyTurn ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


type alias DM =
    Document Msg


view : Model -> DM
view model =
    Document "Invisible Inc."
        [ div
            [ class "center"
            , style "width" (fromInt gridWidthPx ++ "px")
            ]
            [ div [ class "pv2 flex items-center " ]
                [ div [ class "mr3 flex-auto f3" ] [ text (Debug.toString model.status) ]
                , div [ class "" ] [ endTurnButton (model.status /= PlayerTurn) ]
                ]
            , viewGrid model
            ]
        ]


endTurnButton isDisabled =
    button
        [ class "ma0 bn"
        , class "pv2 ph3 br3 f4"
        , style "background-color" "hsl(209, 100%, 79%)"
        , style "color" "hsl(209, 100%, 20%)"
        , onClick EndTurnClicked
        , disabled isDisabled
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


viewGrid model =
    div
        [ style "width" (fromInt gridWidthPx ++ "px")
        , style "height" (fromInt gridHeightPx ++ "px")
        ]
        ((positionsOf gridSize
            |> List.map viewBackgroundTile
         )
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


viewGuard guard =
    let
        pos =
            guard.pos
    in
    div
        [ style "width" (fromInt cellWidthPx ++ "px")
        , style "height" (fromInt cellWidthPx ++ "px")
        , style "transform" (renderCellTransform pos)
        , style "padding" "3px"
        , class "absolute"
        ]
        [ div [ class "w-100 h-100 br3 bg-pink" ] [] ]


viewBackgroundTile pos =
    div
        [ style "width" (fromInt cellWidthPx ++ "px")
        , style "height" (fromInt cellWidthPx ++ "px")
        , style "transform" (renderCellTransform pos)
        , style "padding" "3px"
        , class "absolute"
        ]
        [ div [ class "w-100 h-100 br3 ba bw1 b--light-blue" ] [] ]


renderCellTransform ( x, y ) =
    "translate(" ++ String.fromInt (x * cellWidthPx) ++ "px, " ++ String.fromInt (y * cellWidthPx) ++ "px)"


type alias IntPos =
    ( Int, Int )


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
