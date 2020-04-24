module GameRunner exposing (main)

import Basics.Extra exposing (uncurry)
import Browser exposing (Document)
import GameModel as Game
import Grid exposing (GI, Grid)
import Html exposing (Html, button, div, node, table, text)
import Html.Attributes exposing (autofocus, class, style)
import Html.Events exposing (onClick)
import List.Extra
import PointerEvents as PE
import Process
import Task


updateSelection : GI -> Bool -> Game.Model -> Maybe Game.Model
updateSelection idx wasSelected game =
    let
        stack =
            Game.info game
                |> .selectionStack
    in
    if wasSelected && List.member idx stack then
        -- Remove
        case stack of
            only :: [] ->
                if only == idx then
                    Game.selectionPop game

                else
                    Nothing

            _ :: secondLast :: _ ->
                if secondLast == idx then
                    Game.selectionPop game

                else
                    Nothing

            _ ->
                Nothing

    else if not wasSelected && not (List.member idx stack) then
        -- Add
        Game.selectionPush idx game

    else
        -- NoOp
        Nothing



-- Model


type Model
    = AnimatingMove MoveAnimation
    | Settled SettledState


type SettledState
    = Selecting Game.Model
    | Over Game.Info


type AfterMoveModel
    = AfterMoveSelecting Game.Model
    | AfterMoveOver Game.Info


type alias MoveAnimation =
    { afterMoveModel : AfterMoveModel
    , info : Game.Info
    , context : Game.MoveContext
    , transitionState : MoveTransition
    }


type MoveTransition
    = LeavingTransition
    | FallingTransition
    | EnteringTransition


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( Settled (Selecting Game.init)
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | PlayAnother
    | CollectSelection
    | ToggleSelection GI Bool
    | StepMoveAnimation


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        PlayAnother ->
            case model of
                Settled (Over _) ->
                    init ()

                _ ->
                    ( model, Cmd.none )

        ToggleSelection idx wasSelected ->
            case model of
                Settled (Selecting game) ->
                    let
                        nm =
                            Selecting (updateSelection idx wasSelected game |> Maybe.withDefault game)
                                |> Settled
                    in
                    ( nm, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CollectSelection ->
            case model of
                Settled (Selecting game) ->
                    case Game.makeMove game of
                        Game.InvalidMove ->
                            ( model, Cmd.none )

                        Game.NextState ctx nextGame ->
                            ( AnimatingMove
                                { afterMoveModel = AfterMoveSelecting nextGame
                                , info = Game.info game
                                , context = ctx
                                , transitionState = LeavingTransition
                                }
                            , Process.sleep 1000 |> Task.perform (always StepMoveAnimation)
                            )

                        Game.GameOver _ info ->
                            ( Settled (Over info), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StepMoveAnimation ->
            case model of
                AnimatingMove anim ->
                    case anim.transitionState of
                        LeavingTransition ->
                            ( AnimatingMove { anim | transitionState = FallingTransition }
                            , Process.sleep 1000 |> Task.perform (always StepMoveAnimation)
                            )

                        FallingTransition ->
                            ( AnimatingMove { anim | transitionState = EnteringTransition }
                            , Process.sleep 1000 |> Task.perform (always StepMoveAnimation)
                            )

                        EnteringTransition ->
                            ( (case anim.afterMoveModel of
                                AfterMoveSelecting game ->
                                    Selecting game

                                AfterMoveOver info ->
                                    Over info
                              )
                                |> Settled
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


type alias DM =
    Document Msg


view : Model -> DM
view model =
    Document "GameRunner"
        (node "style" [] [ text """
            body {
                user-select:none;

            }
        """ ]
            :: (case model of
                    Settled (Selecting game) ->
                        [ viewTitle "Game Running"
                        , viewGameInfo (Game.info game)
                        , div [ class "pa3" ] [ btn CollectSelection "collect" ]
                        ]

                    Settled (Over info) ->
                        [ viewTitle "Game Over"
                        , viewGameInfo info
                        , div [ class "pa3" ] [ btn PlayAnother "Play Again?" ]
                        ]

                    AnimatingMove anim ->
                        [ viewTitle (Debug.toString anim.transitionState)
                        , let
                            info =
                                case anim.transitionState of
                                    LeavingTransition ->
                                        { movesLeft = anim.info.movesLeft
                                        , targetSeeds = anim.info.targetSeeds
                                        , targetWater = anim.info.targetWater
                                        , selectionStack = []
                                        , grid = anim.context.collectedGrid
                                        }

                                    FallingTransition ->
                                        { movesLeft = anim.info.movesLeft
                                        , targetSeeds = anim.info.targetSeeds
                                        , targetWater = anim.info.targetWater
                                        , selectionStack = []
                                        , grid = anim.context.fallenGrid
                                        }

                                    EnteringTransition ->
                                        { movesLeft = anim.info.movesLeft
                                        , targetSeeds = anim.info.targetSeeds
                                        , targetWater = anim.info.targetWater
                                        , selectionStack = []
                                        , grid = anim.context.filledGrid
                                        }
                          in
                          viewGameInfo info
                        ]
               )
        )


viewTitle title =
    div [ class "pa3 f3" ] [ text title ]


type alias HM =
    Html Msg


viewGameInfo :
    { a
        | movesLeft : Int
        , targetSeeds : Int
        , targetWater : Int
        , selectionStack : List GI
        , grid : Grid Game.Cell
    }
    -> HM
viewGameInfo i =
    div []
        [ div [ class "pa3" ]
            [ text
                (Debug.toString
                    { movesLeft = i.movesLeft
                    , targetSeeds = i.targetSeeds
                    , targetWater = i.targetWater
                    }
                )
            ]
        , viewGameTable i
        ]


viewGameTable : { a | selectionStack : List GI, grid : Grid Game.Cell } -> HM
viewGameTable info =
    let
        toCellViewModel idx cell =
            let
                selIdx =
                    List.reverse info.selectionStack |> List.Extra.elemIndex idx

                isSelected =
                    selIdx /= Nothing

                selectionMsg =
                    ToggleSelection idx isSelected
            in
            { selectionIdx = selIdx
            , selectionMsg = selectionMsg
            , cell = cell
            }

        gridViewModel =
            info.grid |> Grid.map toCellViewModel
    in
    viewGridAsTable viewCell gridViewModel


type alias CellViewModel a =
    { a
        | selectionIdx : Maybe Int
        , selectionMsg : Msg
        , cell : Game.Cell
    }


viewCell : GI -> CellViewModel a -> HM
viewCell _ vm =
    div
        [ PE.onPrimaryEnterAndDown vm.selectionMsg
        , PE.onPrimaryDown vm.selectionMsg
        , class "br3 w3 h3 flex"
        , style "transition" "transform 300ms"
        , style "transform"
            (if vm.selectionIdx == Nothing then
                "scale(1.0)"

             else
                "scale(0.8)"
            )
        , class
            (case vm.cell of
                Game.Water ->
                    "bg-light-blue"

                Game.Wall ->
                    "bg-light-purple white"

                Game.Empty ->
                    ""

                Game.Seed ->
                    "bg-light-pink "
            )
        ]
        [ text
            (vm.selectionIdx
                |> Maybe.map String.fromInt
                |> Maybe.withDefault ""
            )
        ]


viewGridAsTable : (GI -> a -> HM) -> Grid.Grid a -> HM
viewGridAsTable renderCell grid =
    let
        cells =
            Grid.toList grid

        rows =
            List.Extra.gatherEqualsBy (Tuple.first >> Tuple.second) cells
                |> List.map (uncurry (::))

        renderCell_ ( gi, a ) =
            Html.td [] [ renderCell gi a ]

        viewGridRow : Int -> List ( GI, a ) -> HM
        viewGridRow y entry =
            Html.tr []
                (viewYTH y :: List.map renderCell_ entry)

        rowWidth =
            List.head rows |> Maybe.map List.length |> Maybe.withDefault 0

        styledGridTH =
            Html.th [ class "code f4 pa1" ]

        viewXTH x =
            styledGridTH [ text ("x" ++ String.fromInt x) ]

        viewYTH y =
            styledGridTH [ text ("y" ++ String.fromInt y) ]

        gridHeaderRow : Int -> HM
        gridHeaderRow gridWidth =
            Html.tr []
                (styledGridTH [ text "x,y" ]
                    :: List.map viewXTH (rangeLen gridWidth)
                )
    in
    table [ class "pa3" ]
        (gridHeaderRow rowWidth :: List.indexedMap viewGridRow rows)


rangeLen len =
    List.range 0 (len - 1)


btn msg txt =
    button
        [ onClick msg, class "ma2", autofocus True ]
        [ text txt ]



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
