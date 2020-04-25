module GameRunner exposing (main)

import Basics.Extra exposing (uncurry)
import Browser exposing (Document)
import Dict
import GameModel as Game
import Grid exposing (GI, Grid)
import Html exposing (Html, button, node, table, text)
import Html.Attributes exposing (autofocus, class, style)
import Html.Events exposing (onClick)
import List.Extra
import PointerEvents as PE
import Process
import Set
import Task


updateSelection : GI -> Bool -> Game.Model -> Maybe Game.Model
updateSelection idx wasSelected game =
    let
        stack =
            Game.selectionStack game
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



-- TRANSITION STEPS


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
    | Over Game.Stats Game.CellGrid


type alias MoveAnimation =
    { settledState : SettledState
    , initialGrid : Grid Game.Cell
    , stats : Game.Stats
    , moveDetails : Game.MoveDetails
    , steps : TransitionSteps MoveTransition
    }


type MoveTransition
    = LeavingTransition
    | EnteringStartTransition
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
                Settled (Over _ _) ->
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
            let
                initMoveTransitionSteps =
                    initTS StepMoveAnimation
                        ( LeavingTransition, 300 )
                        [ ( EnteringStartTransition, 20 )
                        , ( EnteringTransition, 300 )
                        ]
            in
            case model of
                Settled (Selecting game) ->
                    case Game.makeMove game of
                        Game.InvalidMove ->
                            ( model, Cmd.none )

                        Game.NextModel ctx nextGame ->
                            let
                                ( transitionSteps, cmd ) =
                                    initMoveTransitionSteps
                            in
                            ( AnimatingMove
                                { settledState = Selecting nextGame
                                , initialGrid = Game.cellGrid game
                                , stats = Game.stats nextGame
                                , moveDetails = ctx
                                , steps = transitionSteps
                                }
                            , cmd
                            )

                        Game.GameOver ctx stats ->
                            let
                                ( transitionSteps, cmd ) =
                                    initMoveTransitionSteps
                            in
                            ( AnimatingMove
                                { settledState = Over stats ctx.generated.grid
                                , initialGrid = Game.cellGrid game
                                , stats = stats
                                , moveDetails = ctx
                                , steps = transitionSteps
                                }
                            , cmd
                            )

                _ ->
                    ( model, Cmd.none )

        StepMoveAnimation ->
            case model of
                AnimatingMove anim ->
                    case stepTS StepMoveAnimation anim.steps of
                        Just ( transitionSteps, cmd ) ->
                            ( AnimatingMove { anim | steps = transitionSteps }
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


type alias HM =
    Html Msg


type alias AM =
    Html.Attribute Msg


div : List AM -> List HM -> HM
div =
    Html.div


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
                        , viewGameStats (Game.stats game)
                        , viewCellGridTableWithSelectionStack (Game.selectionStack game) (Game.cellGrid game)
                        , div [ class "pa3" ] [ btn CollectSelection "collect" ]
                        ]

                    Settled (Over stats grid) ->
                        [ viewTitle "Game Over"
                        , viewGameStats stats
                        , viewCellGridTableWithSelectionStack [] grid
                        , div [ class "pa3" ] [ btn PlayAnother "Play Again?" ]
                        ]

                    AnimatingMove anim ->
                        [ viewTitle (Debug.toString (currentTS anim.steps))
                        , viewGameStats anim.stats
                        , viewCellGridTableWithMoveAnimation anim
                        ]
               )
        )


viewTitle : String -> HM
viewTitle title =
    div [ class "pa3 f3" ] [ text title ]


viewGameStats : Game.Stats -> HM
viewGameStats stats =
    div [ class "pa3" ] [ text (Debug.toString stats) ]


viewCellGridTableWithMoveAnimation : MoveAnimation -> HM
viewCellGridTableWithMoveAnimation anim =
    let
        toCellVMHelp : (GI -> CellState) -> GI -> Game.Cell -> CellViewModel
        toCellVMHelp func idx cell =
            { selectionIdx = Nothing
            , selectionMsg = Nothing
            , cell = cell
            , cellState = func idx
            }

        ( grid, idxToCS ) =
            case currentTS anim.steps |> Tuple.first of
                LeavingTransition ->
                    ( anim.initialGrid
                    , let
                        idxToCellState idx =
                            if Set.member idx anim.moveDetails.collected.indexSet then
                                CellLeaving

                            else
                                case Dict.get idx anim.moveDetails.fallenLookup of
                                    Just to ->
                                        CellFallingTo to

                                    Nothing ->
                                        CellStatic
                      in
                      idxToCellState
                    )

                EnteringStartTransition ->
                    ( anim.moveDetails.generated.grid
                    , let
                        idxToCellState idx =
                            if Set.member idx anim.moveDetails.generated.indexSet then
                                CellEnterStart

                            else
                                CellStaticNoTransition
                      in
                      idxToCellState
                    )

                EnteringTransition ->
                    ( anim.moveDetails.generated.grid
                    , always CellStatic
                    )
    in
    viewCellGridTable (Grid.map (toCellVMHelp idxToCS) grid)


viewCellGridTableWithSelectionStack : List GI -> Game.CellGrid -> HM
viewCellGridTableWithSelectionStack selectionStack grid =
    let
        toCellViewModel : GI -> Game.Cell -> CellViewModel
        toCellViewModel idx cell =
            let
                selIdx =
                    List.reverse selectionStack |> List.Extra.elemIndex idx

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
            grid |> Grid.map toCellViewModel
    in
    viewCellGridTable gridViewModel


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
    | CellFallingTo GI
    | CellEnterStart
    | CellStaticNoTransition


maybeAttr : (a -> Html.Attribute msg) -> Maybe a -> Html.Attribute msg
maybeAttr attrFunc maybeValue =
    case maybeValue of
        Just val ->
            attrFunc val

        Nothing ->
            class ""


viewCellGridTable : Grid CellViewModel -> HM
viewCellGridTable =
    viewGridAsTable viewCell


viewCell : GI -> CellViewModel -> HM
viewCell idx vm =
    let
        defaultTransitionStyle =
            style "transition" "transform 300ms"

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

                CellFallingTo toIdx ->
                    let
                        dyFactor =
                            (Tuple.second toIdx - Tuple.second idx)
                                |> String.fromInt

                        translateStr =
                            "translate(0, calc( " ++ dyFactor ++ " * 4.50rem ) )"
                    in
                    [ style "transform" ([ translateStr, "scale(1)" ] |> String.join " ")
                    , defaultTransitionStyle
                    ]

                CellEnterStart ->
                    [ style "transform" "translate(0,-300px) scale(0)"
                    , style "transition" "none"
                    ]

                CellStaticNoTransition ->
                    [ style "transform" "translate(0,0) scale(1.0)"
                    , style "transition" "none"
                    ]
    in
    div
        (animProps
            ++ [ maybeAttr PE.onPrimaryEnterAndDown vm.selectionMsg
               , maybeAttr PE.onPrimaryDown vm.selectionMsg
               , class "ma1 br3 w3 h3 flex"
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
            Html.td [ class "pa0" ] [ renderCell gi a ]

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
    table [ class "pa3", style "border-collapse" "collapse" ]
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
