module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
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
    , triggerStep
    )


triggerStep =
    Process.sleep 1000 |> Task.perform (always Step)



-- Update


type Msg
    = NoOp
    | Step


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Step ->
            ( { model | guard = moveGuard model.guard }, Cmd.none )


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
            [ div [ class "pv2 f3" ] [ text (Debug.toString model.status) ]
            , viewGrid model
            ]
        ]


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
