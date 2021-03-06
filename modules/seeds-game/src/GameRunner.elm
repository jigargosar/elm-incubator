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



-- Model


type Model
    = AnimatingMove MoveAnimation
    | Settled Game.Model


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( Settled Game.init
    , Cmd.none
    )



-- MOVE ANIMATION


type alias MoveAnimation =
    { game : Game.Model
    , moveDetails : Game.MoveDetails
    , steps : TransitionSteps MoveAnimationStage
    }


type MoveAnimationStage
    = CellsLeavingAndFallingAnimationStage
    | CellsEnteringAnimationStage


initMoveAnimation : Game.MoveDetails -> Game.Model -> ( Model, Cmd Msg )
initMoveAnimation moveDetails nextGame =
    let
        ( transitionSteps, cmd ) =
            initTS StepMoveAnimationStage
                ( CellsLeavingAndFallingAnimationStage, 300 )
                [ ( CellsEnteringAnimationStage, 300 )
                ]
    in
    ( AnimatingMove
        { game = nextGame
        , moveDetails = moveDetails
        , steps = transitionSteps
        }
    , cmd
    )



-- TRANSITION STEPS


type TransitionSteps a
    = TransitionSteps ( a, Float ) (List ( a, Float ))


initTS : msg -> ( a, Float ) -> List ( a, Float ) -> ( TransitionSteps a, Cmd msg )
initTS msg current rest =
    ( TransitionSteps current rest, delay (Tuple.second current) msg )


delay : Float -> msg -> Cmd msg
delay for msg =
    Process.sleep for |> Task.perform (always msg)


updateTS : msg -> TransitionSteps a -> Maybe ( TransitionSteps a, Cmd msg )
updateTS msg (TransitionSteps _ steps) =
    case steps of
        [] ->
            Nothing

        current :: rest ->
            Just (initTS msg current rest)


currentTS : TransitionSteps a -> ( a, Float )
currentTS (TransitionSteps current _) =
    current



-- Update


type Msg
    = NoOp
    | PlayAnother
    | CollectSelection
    | ToggleSelection GI Bool
    | StepMoveAnimationStage


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        PlayAnother ->
            case model of
                Settled game ->
                    if Game.isOver game then
                        init ()

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleSelection idx wasSelected ->
            case model of
                Settled game ->
                    ( Settled
                        (updateSelection idx wasSelected game
                            |> Maybe.withDefault game
                        )
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        CollectSelection ->
            case model of
                Settled game ->
                    case Game.makeMove game of
                        Nothing ->
                            ( model, Cmd.none )

                        Just ( moveDetails, nextGame ) ->
                            initMoveAnimation moveDetails nextGame

                _ ->
                    ( model, Cmd.none )

        StepMoveAnimationStage ->
            case model of
                AnimatingMove anim ->
                    case updateTS StepMoveAnimationStage anim.steps of
                        Just ( transitionSteps, cmd ) ->
                            ( AnimatingMove { anim | steps = transitionSteps }, cmd )

                        Nothing ->
                            ( Settled anim.game, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- UPDATE SELECTION


updateSelection : GI -> Bool -> Game.Model -> Maybe Game.Model
updateSelection idx wasSelected model =
    let
        stack =
            Game.selectionStack model
    in
    if wasSelected && List.member idx stack then
        -- Remove
        case stack of
            only :: [] ->
                if only == idx then
                    Game.selectionPop model

                else
                    Nothing

            _ :: secondLast :: _ ->
                if secondLast == idx then
                    Game.selectionPop model

                else
                    Nothing

            _ ->
                Nothing

    else if not wasSelected && not (List.member idx stack) then
        -- Add
        Game.selectionPush idx model

    else
        -- NoOp
        Nothing



-- SUBSCRIPTIONS


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
                    Settled game ->
                        if Game.isOver game then
                            [ viewTitle "Game Over"
                            , viewGameStats (Game.stats game)
                            , viewCellGridTable
                                (selectionStackToCellGridViewModel
                                    (Game.selectionStack game)
                                    (Game.cellGrid game)
                                )
                            , div [ class "pa3" ] [ btn PlayAnother "Play Again?" ]
                            ]

                        else
                            [ viewTitle "Game Running"
                            , viewGameStats (Game.stats game)
                            , viewCellGridTable
                                (selectionStackToCellGridViewModel
                                    (Game.selectionStack game)
                                    (Game.cellGrid game)
                                )
                            , div [ class "pa3" ] [ btn CollectSelection "collect" ]
                            ]

                    AnimatingMove anim ->
                        [ viewTitle (Debug.toString (currentTS anim.steps))
                        , viewGameStats (Game.stats anim.game)
                        , viewCellGridTable
                            (moveTransitionToCellGridViewModel
                                anim.moveDetails
                                (currentTS anim.steps |> Tuple.first)
                            )
                        ]
               )
        )


viewTitle : String -> HM
viewTitle title =
    div [ class "pa3 f3" ] [ text title ]


viewGameStats : Game.Stats -> HM
viewGameStats stats =
    div [ class "pa3" ] [ text (Debug.toString stats) ]


type alias CellGridViewModel =
    Grid CellViewModel


moveTransitionToCellGridViewModel : Game.MoveDetails -> MoveAnimationStage -> CellGridViewModel
moveTransitionToCellGridViewModel moveDetails moveTransition =
    case moveTransition of
        CellsLeavingAndFallingAnimationStage ->
            let
                idxToCellState idx =
                    if Set.member idx moveDetails.collected.indexSet then
                        CellLeavingAnimation

                    else
                        case Dict.get idx moveDetails.fallen.lookup of
                            Just to ->
                                CellFallingAnimation to

                            Nothing ->
                                CellNoAnimation
            in
            Grid.map
                (\idx cell ->
                    { selectionMsg = Nothing
                    , cell = cell
                    , animation = idxToCellState idx
                    , selectionState = toCellSelectionState moveDetails.initial.selectionStack idx
                    }
                )
                moveDetails.initial.grid

        CellsEnteringAnimationStage ->
            let
                idxToCellState idx =
                    if Set.member idx moveDetails.generated.indexSet then
                        CellEnteringAnimation

                    else
                        CellNoAnimation
            in
            Grid.map
                (\idx cell ->
                    { selectionMsg = Nothing
                    , cell = cell
                    , animation = idxToCellState idx
                    , selectionState = toCellSelectionState [] idx
                    }
                )
                moveDetails.generated.grid


selectionStackToCellGridViewModel : List GI -> Game.CellGrid -> CellGridViewModel
selectionStackToCellGridViewModel selectionStack =
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
            { selectionMsg = Just selectionMsg
            , cell = cell
            , animation = CellNoAnimation
            , selectionState = toCellSelectionState selectionStack idx
            }
    in
    Grid.map toCellViewModel


toCellSelectionState : List GI -> GI -> CellSelectionState
toCellSelectionState selectionStack idx =
    case List.reverse selectionStack |> List.Extra.elemIndex idx of
        Just selectionIndex ->
            CellSelectionActive selectionIndex

        Nothing ->
            CellSelectionInactive


type alias CellViewModel =
    { selectionMsg : Maybe Msg
    , cell : Game.Cell
    , animation : CellAnimation
    , selectionState : CellSelectionState
    }


type CellSelectionState
    = CellSelectionActive Int
    | CellSelectionInactive


type CellAnimation
    = CellNoAnimation
    | CellLeavingAnimation
    | CellFallingAnimation GI
    | CellEnteringAnimation


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


styles : List ( String, String ) -> Html.Attribute msg
styles =
    List.map (\( n, v ) -> n ++ ": " ++ v)
        >> String.join ";"
        >> Html.Attributes.attribute "style"


viewCell : GI -> CellViewModel -> HM
viewCell idx vm =
    let
        animProps =
            case vm.animation of
                CellNoAnimation ->
                    []

                CellLeavingAnimation ->
                    [ class "cell_leave" ]

                CellFallingAnimation toIdx ->
                    let
                        dyFactor =
                            (Tuple.second toIdx - Tuple.second idx)
                                |> String.fromInt
                    in
                    [ class "cell_fall"
                    , styles [ ( "--cell-fall-dy", "calc( " ++ dyFactor ++ " * 4.50rem )" ) ]
                    ]

                CellEnteringAnimation ->
                    [ class "cell_enter" ]
    in
    div
        (animProps
            ++ [ maybeAttr PE.onPrimaryEnterAndDown vm.selectionMsg
               , maybeAttr PE.onPrimaryDown vm.selectionMsg
               , case vm.selectionState of
                    CellSelectionActive _ ->
                        class "cell_selection_active"

                    CellSelectionInactive ->
                        class "cell_selection_inactive"
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
            (case vm.selectionState of
                CellSelectionActive selIdx ->
                    String.fromInt selIdx

                CellSelectionInactive ->
                    ""
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
