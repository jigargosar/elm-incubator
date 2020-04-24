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



-- TRANSITION STATE HELPER


type TransitionSteps a
    = TransitionSteps ( a, Float ) (List ( a, Float ))


initTS : msg -> ( a, Float ) -> List ( a, Float ) -> ( TransitionSteps a, Cmd msg )
initTS msg current rest =
    ( TransitionSteps current rest, delay (Tuple.second current) msg )


delay : Float -> msg -> Cmd msg
delay for msg =
    Process.sleep for |> Task.perform (always msg)


stepTS : msg -> TransitionSteps a -> Maybe ( TransitionSteps a, Cmd msg )
stepTS msg (TransitionSteps _ steps) =
    case steps of
        [] ->
            Nothing

        current :: rest ->
            Just (initTS msg current rest)


currentTS : TransitionSteps a -> ( a, Float )
currentTS (TransitionSteps current _) =
    current



-- Model


type Model
    = AnimatingMove MoveAnimation
    | Settled SettledState


type SettledState
    = Selecting Game.Model
    | Over Game.Info


type alias MoveAnimation =
    { settledState : SettledState
    , info : Game.Info
    , context : Game.MoveContext
    , transitionSteps : TransitionSteps MoveTransition
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
                            let
                                ( transitionSteps, cmd ) =
                                    initTS StepMoveAnimation
                                        ( LeavingTransition, 1000 )
                                        [ ( FallingTransition, 1000 )
                                        , ( EnteringTransition, 1000 )
                                        ]
                            in
                            ( AnimatingMove
                                { settledState = Selecting nextGame
                                , info = Game.info game
                                , context = ctx
                                , transitionSteps = transitionSteps
                                }
                            , cmd
                            )

                        Game.GameOver _ info ->
                            ( Settled (Over info), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StepMoveAnimation ->
            case model of
                AnimatingMove anim ->
                    case stepTS StepMoveAnimation anim.transitionSteps of
                        Just ( transitionSteps, cmd ) ->
                            ( AnimatingMove { anim | transitionSteps = transitionSteps }
                            , cmd
                            )

                        Nothing ->
                            ( Settled anim.settledState
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
                        [ viewTitle (Debug.toString (currentTS anim.transitionSteps))
                        , let
                            ( info, toCellViewModel ) =
                                case currentTS anim.transitionSteps |> Tuple.first of
                                    LeavingTransition ->
                                        ( { movesLeft = anim.info.movesLeft
                                          , targetSeeds = anim.info.targetSeeds
                                          , targetWater = anim.info.targetWater
                                          , selectionStack = []
                                          , grid = anim.context.beforeGrid
                                          }
                                        , let
                                            toCellViewModel_ : GI -> Game.Cell -> CellViewModel
                                            toCellViewModel_ idx cell =
                                                { selectionIdx = Nothing
                                                , selectionMsg = Nothing
                                                , cell = cell
                                                , cellState =
                                                    if List.member ( idx, cell ) anim.context.collectedEntries then
                                                        CellLeaving

                                                    else
                                                        CellStatic
                                                }
                                          in
                                          toCellViewModel_
                                        )

                                    FallingTransition ->
                                        ( { movesLeft = anim.info.movesLeft
                                          , targetSeeds = anim.info.targetSeeds
                                          , targetWater = anim.info.targetWater
                                          , selectionStack = []
                                          , grid = anim.context.fallenGrid
                                          }
                                        , let
                                            toCellViewModel_ : GI -> Game.Cell -> CellViewModel
                                            toCellViewModel_ _ cell =
                                                { selectionIdx = Nothing
                                                , selectionMsg = Nothing
                                                , cell = cell
                                                , cellState = CellStatic
                                                }
                                          in
                                          toCellViewModel_
                                        )

                                    EnteringTransition ->
                                        ( { movesLeft = anim.info.movesLeft
                                          , targetSeeds = anim.info.targetSeeds
                                          , targetWater = anim.info.targetWater
                                          , selectionStack = []
                                          , grid = anim.context.filledGrid
                                          }
                                        , let
                                            toCellViewModel_ : GI -> Game.Cell -> CellViewModel
                                            toCellViewModel_ _ cell =
                                                { selectionIdx = Nothing
                                                , selectionMsg = Nothing
                                                , cell = cell
                                                , cellState = CellStatic
                                                }
                                          in
                                          toCellViewModel_
                                        )
                          in
                          div []
                            [ div [ class "pa3" ]
                                [ text
                                    (Debug.toString
                                        { movesLeft = info.movesLeft
                                        , targetSeeds = info.targetSeeds
                                        , targetWater = info.targetWater
                                        }
                                    )
                                ]
                            , let
                                gridViewModel : Grid CellViewModel
                                gridViewModel =
                                    info.grid |> Grid.map toCellViewModel
                              in
                              viewGridAsTable viewCell gridViewModel
                            ]
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
        toCellViewModel : GI -> Game.Cell -> CellViewModel
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
            , selectionMsg = Just selectionMsg
            , cell = cell
            , cellState =
                if isSelected then
                    CellSelected

                else
                    CellStatic
            }

        gridViewModel : Grid CellViewModel
        gridViewModel =
            info.grid |> Grid.map toCellViewModel
    in
    viewGridAsTable viewCell gridViewModel


type alias CellViewModel =
    { selectionIdx : Maybe Int
    , selectionMsg : Maybe Msg
    , cell : Game.Cell
    , cellState : CellState
    }


type CellState
    = CellStatic
    | CellSelected
    | CellLeaving


maybeAttr : (a -> Html.Attribute msg) -> Maybe a -> Html.Attribute msg
maybeAttr attrFunc maybeValue =
    case maybeValue of
        Just val ->
            attrFunc val

        Nothing ->
            class ""


viewCell : GI -> CellViewModel -> HM
viewCell _ vm =
    let
        defaultTransitionStyle =
            style "transition" "transform 500ms"

        animProps =
            case vm.cellState of
                CellStatic ->
                    [ style "transform" "translate(0,0) scale(1.0)"
                    , defaultTransitionStyle
                    ]

                CellSelected ->
                    [ style "transform" "translate(0,0) scale(0.8)"
                    , defaultTransitionStyle
                    ]

                CellLeaving ->
                    [ style "transform" "translate(0,-300px) scale(0)"
                    , defaultTransitionStyle
                    ]
    in
    div
        (animProps
            ++ [ maybeAttr PE.onPrimaryEnterAndDown vm.selectionMsg
               , maybeAttr PE.onPrimaryDown vm.selectionMsg
               , class "br3 w3 h3 flex"
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
        )
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
