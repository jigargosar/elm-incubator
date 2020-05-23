port module Main exposing (main)

import AStar
import Browser exposing (Document)
import Browser.Events
import Cons exposing (Cons)
import Html exposing (Attribute, Html, button, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import IntPos exposing (IntPos)
import IntSize exposing (IntSize)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JDX
import Json.Decode.Pipeline exposing (required)
import Json.Encode as JE exposing (Value)
import List.Extra as List
import More exposing (..)
import MouseEvents as ME
import Process
import Set exposing (Set)
import Svg
import Svg.Attributes as SA exposing (fill, stroke)
import Task
import TypedSvg.Attributes as TSA
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types as TST
import XY exposing (XY)


port cache : String -> Cmd msg



-- IntPos


isMemberOfSize : IntSize -> IntPos -> Bool
isMemberOfSize s p =
    IntSize.member p s



-- Guard


type alias Guard =
    { state : GuardState
    }


type GuardState
    = Patrolling IntPos IntPos
    | KnockedOut IntPos Int


positionOfGuard : Guard -> IntPos
positionOfGuard guard =
    case guard.state of
        Patrolling s _ ->
            s

        KnockedOut p _ ->
            p


koValueOfGuard : Guard -> Maybe Int
koValueOfGuard guard =
    case guard.state of
        Patrolling _ _ ->
            Nothing

        KnockedOut _ ko ->
            Just ko



--targetPositionOfGuard : Guard -> IntPos
--targetPositionOfGuard =
--    .path >> LZ.last >> LZ.current


initGuard : IntPos -> Guard
initGuard pos =
    { state = Patrolling pos pos
    }


guardSetStartPosition : IntPos -> Guard -> Guard
guardSetStartPosition s guard =
    case guard.state of
        Patrolling _ e ->
            { guard | state = Patrolling s e }

        KnockedOut _ _ ->
            guard


guardSetEndPosition : IntPos -> Guard -> Guard
guardSetEndPosition e guard =
    case guard.state of
        Patrolling s _ ->
            { guard | state = Patrolling s e }

        KnockedOut _ _ ->
            guard


stepGuard : Guard -> ( Bool, Guard )
stepGuard guard =
    ( True, guard )



-- Model


gridSize =
    { width = 10
    , height = 15
    }


type alias Model =
    { guard : Guard
    , agent : IntPos
    , status : Status
    , walls : Set IntPos
    , edit : Edit
    , hover : IntPos
    }


occupied : Model -> Set IntPos
occupied model =
    model.walls
        |> Set.insert (positionOfGuard model.guard)
        |> Set.insert model.agent


isOccupied : IntPos -> Model -> Bool
isOccupied pos model =
    Set.member pos (occupied model)


isValidToggleWallGridPos : IntPos -> Model -> Bool
isValidToggleWallGridPos pos model =
    IntSize.member pos gridSize
        && (occupied model
                |> setRemoveAll model.walls
                |> Set.member pos
                |> not
           )


unOccupiedNeighbours : Model -> IntPos -> Set IntPos
unOccupiedNeighbours =
    occupied >> unOccupiedNeighboursHelp


unOccupiedNeighboursHelp : Set IntPos -> IntPos -> Set IntPos
unOccupiedNeighboursHelp blocked pos =
    IntSize.adjacentMembers pos gridSize
        |> Set.fromList
        |> setRemoveAll blocked


type Status
    = PlayerTurn
    | EnemyTurn EnemyTurnModel


type alias EnemyTurnModel =
    { guardPath : List IntPos }


type Edit
    = EditWall
    | EditGuard
    | EditGuardDest
    | EditNone


type alias Flags =
    { cache : String }


modelDecoder : Decoder Model
modelDecoder =
    JD.succeed initModel
        |> required "wallPositions" (JDX.set intPosDecoder)


modelEncoder : Model -> Value
modelEncoder model =
    JE.object <|
        [ ( "wallPositions", JE.set intPosEncoder model.walls )
        ]


intPosDecoder : Decoder IntPos
intPosDecoder =
    JD.map2 pair (JD.index 0 JD.int) (JD.index 1 JD.int)


intPosEncoder : IntPos -> Value
intPosEncoder intPos =
    (\( a, b ) -> JE.list identity [ JE.int a, JE.int b ]) intPos


initModel : Set IntPos -> Model
initModel walls =
    let
        guardPos =
            ( 3, 3 )

        agentPos =
            ( 5, 5 )

        filteredWalls =
            walls
                |> Set.remove guardPos
                |> Set.remove agentPos

        model : Model
        model =
            { guard = initGuard guardPos
            , agent = agentPos
            , status = PlayerTurn
            , walls = filteredWalls
            , edit = EditNone
            , hover = ( 0, 0 )
            }
    in
    model
        |> mapGuard (guardSetEndPosition ( 5, 12 ))


init : Flags -> ( Model, Cmd Msg )
init flags =
    case JD.decodeString modelDecoder flags.cache of
        Ok model ->
            ( model
                |> mapWalls (Set.filter (isMemberOfSize gridSize))
                |> mapWalls (Set.filter (neq (positionOfGuard model.guard)))
            , Cmd.none
            )

        Err _ ->
            ( initModel Set.empty
            , Cmd.none
            )


triggerStepEnemyTurn =
    Process.sleep 100 |> Task.perform (always StepEnemyTurn)



-- Update


type Msg
    = NoOp
    | SetEdit Edit
    | StepEnemyTurn
    | EndTurnClicked
    | GridPosClicked IntPos
    | GridPosHovered IntPos Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        SetEdit edit ->
            ( { model | edit = edit }, Cmd.none )

        StepEnemyTurn ->
            case model.status of
                PlayerTurn ->
                    ( model, Cmd.none )

                EnemyTurn et ->
                    let
                        ( isDone, guard ) =
                            stepGuard model.guard

                        _ =
                            case et.guardPath of
                                [] ->
                                    ( model, Cmd.none )

                                h :: t ->
                                    ( model, Cmd.none )
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
                    ( { model
                        | status =
                            EnemyTurn
                                { guardPath =
                                    model
                                        |> guardPredictedPath
                                        |> Cons.tail
                                }
                      }
                    , triggerStepEnemyTurn
                    )

                EnemyTurn _ ->
                    ( model, Cmd.none )

        GridPosClicked pos ->
            if IntSize.member pos gridSize then
                updateOnPosMouseDown pos model
                    |> withEffect (cacheIfChanged model)

            else
                ( model, Cmd.none )

        GridPosHovered hover isPrimaryDown ->
            ( (if
                isPrimaryDown
                    && (model.hover /= hover)
                    && (model.edit == EditWall)
                    && isValidToggleWallGridPos hover model
               then
                model
                    |> mapWalls (toggleSetMember hover)

               else
                model
              )
                |> setHover hover
            , Cmd.none
            )


setHover : IntPos -> Model -> Model
setHover hover model =
    { model | hover = hover }


updateOnPosMouseDown : IntPos -> Model -> Model
updateOnPosMouseDown pos model =
    if
        not (isGuardSelected model)
            && (positionOfGuard model.guard == pos)
    then
        setEdit EditGuard model

    else if
        not (isAgentSelected model)
            && (model.agent == pos)
    then
        setEdit EditNone model

    else
        case model.edit of
            EditWall ->
                if isValidToggleWallGridPos pos model then
                    model
                        |> mapWalls (toggleSetMember pos)

                else
                    model

            EditGuard ->
                if isOccupied pos model then
                    model

                else
                    model
                        |> mapGuard (guardSetStartPosition pos)
                        |> setEdit EditGuardDest

            EditGuardDest ->
                model
                    |> mapGuard (guardSetEndPosition pos)
                    |> setEdit EditGuard

            EditNone ->
                let
                    ap =
                        agentHoverPath model
                in
                if Cons.last ap == pos then
                    { model | agent = pos }

                else
                    model


withEffect : (b -> a) -> b -> ( b, a )
withEffect f model =
    ( model, f model )


toggleSetMember : comparable -> Set comparable -> Set comparable
toggleSetMember x xs =
    if Set.member x xs then
        Set.remove x xs

    else
        Set.insert x xs


mapWalls : (Set IntPos -> Set IntPos) -> Model -> Model
mapWalls f model =
    { model | walls = f model.walls }


mapGuard : (Guard -> Guard) -> Model -> Model
mapGuard f model =
    { model | guard = f model.guard }


setEdit : Edit -> Model -> Model
setEdit edit model =
    { model | edit = edit }


cacheCmd : Model -> Cmd msg
cacheCmd =
    modelEncoder >> JE.encode 0 >> cache


cacheIfChanged : Model -> Model -> Cmd msg
cacheIfChanged oldModel model =
    if oldModel /= model then
        cacheCmd model

    else
        Cmd.none


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown
            (JD.field "key" JD.string
                |> JD.andThen
                    (keyToMsg >> unwrap (JD.fail "ignore") JD.succeed)
            )
        ]


keyToMsg : String -> Maybe Msg
keyToMsg key =
    [ ( " ", EndTurnClicked )
    , ( "w", SetEdit EditWall )
    , ( "g", SetEdit EditGuard )
    , ( "Escape", SetEdit EditNone )
    ]
        |> List.find (fst >> eq key)
        |> Maybe.map snd



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
            , wpx gridWidthPx
            ]
            [ div [ class "pv2 flex items-center " ]
                [ div [ class "mr3 flex-auto f3" ]
                    [ span []
                        [ text (Debug.toString model.status)
                        , text " "
                        , text (Debug.toString model.edit)
                        ]
                    ]
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


cellWidthPx =
    50


innerCellWidth =
    cellWidthPx - 10


gridWidthPx =
    cellWidthPx * gridSize.width


gridHeightPx =
    cellWidthPx * gridSize.height


toGridPos : XY -> IntPos
toGridPos xy =
    ( floor (xy.x / cellWidthPx)
    , floor (xy.y / cellWidthPx)
    )


mouseGridPosDecoder : Decoder IntPos
mouseGridPosDecoder =
    JD.map2 XY.sub
        XY.pageXYDecoder
        XY.currentTargetOffsetXYDecoder
        |> JD.map toGridPos


isAgentSelected : Model -> Bool
isAgentSelected model =
    case model.edit of
        EditWall ->
            False

        EditGuard ->
            False

        EditGuardDest ->
            False

        EditNone ->
            True


isGuardSelected : Model -> Bool
isGuardSelected model =
    case model.edit of
        EditWall ->
            False

        EditGuard ->
            True

        EditGuardDest ->
            True

        EditNone ->
            False


viewGrid : Model -> HM
viewGrid model =
    div
        [ ME.down (JD.map GridPosClicked mouseGridPosDecoder)
        , ME.over (JD.map2 GridPosHovered mouseGridPosDecoder (JD.field "buttons" (JD.map (eq 1) JD.int)))
        ]
        [ Svg.svg
            [ TSA.viewBox (gridWidthPx * -0.5) (gridHeightPx * -0.5) gridWidthPx gridHeightPx
            , noFill
            ]
            ((IntSize.positions gridSize
                |> List.map viewBackgroundTile
             )
                ++ (model.walls |> Set.toList |> List.map viewWall)
                ++ [ viewAgent (isAgentSelected model)
                        model.agent
                   , renderGuardView (toGuardView model)
                   ]
                ++ viewGuardPath (guardPredictedPath model)
                ++ (case isAgentSelected model of
                        True ->
                            viewHoverPath (agentHoverPath model)

                        False ->
                            []
                   )
            )
        ]


agentHoverPath : Model -> Cons IntPos
agentHoverPath model =
    computePath (unOccupiedNeighbours model) model.agent model.hover


guardPredictedPath : Model -> Cons IntPos
guardPredictedPath model =
    case model.guard.state of
        Patrolling s e ->
            computePath (unOccupiedNeighbours model) s e

        KnockedOut p _ ->
            Cons.singleton p


computePath : (IntPos -> Set IntPos) -> IntPos -> IntPos -> Cons IntPos
computePath mv startPos endPos =
    AStar.findPath AStar.pythagoreanCost mv startPos endPos
        |> Maybe.withDefault []
        |> List.take 10
        |> Cons.init startPos


viewAgent isSelected pos =
    group [ svgCellTransform pos ]
        [ square innerCellWidth
            [ SA.fill "hsl(209, 100%, 79%)"
            , SA.rx "10"
            ]
        , viewSelectionOutline isSelected
        ]


type alias GuardView =
    { pos : IntPos
    , selected : Bool
    , ko : Maybe Int
    }


toGuardView : Model -> GuardView
toGuardView model =
    { pos = positionOfGuard model.guard
    , selected = isGuardSelected model
    , ko = koValueOfGuard model.guard
    }


renderGuardView : GuardView -> HM
renderGuardView gv =
    let
        pos =
            gv.pos
    in
    group [ svgCellTransform pos ]
        [ square innerCellWidth
            [ SA.fill "hsl(324, 100%, 75%)"
            , SA.rx "10"
            ]
        , viewSelectionOutline gv.selected
        , gv.ko
            |> unwrapView
                (\ko ->
                    Svg.text_
                        [ fill "black"
                        , SA.textAnchor "middle"
                        , SA.dominantBaseline "central"
                        , TSA.transform [ TST.Translate 0 -10, TST.Scale 0.8 0.8 ]
                        ]
                        [ text <| "KO=" ++ intS ko ]
                )
        ]



--viewGuard : Bool -> Guard -> HM
--viewGuard isSelected guard =
--    let
--        pos =
--            positionOfGuard guard
--    in
--    group [ svgCellTransform pos ]
--        [ square innerCellWidth
--            [ SA.fill "hsl(324, 100%, 75%)"
--            , SA.rx "10"
--            ]
--        , viewSelectionOutline isSelected
--        , case guard.state of
--            KnockedOut _ ko ->
--                Svg.text_
--                    [ fill "black"
--                    , SA.textAnchor "middle"
--                    , SA.dominantBaseline "central"
--                    , TSA.transform [ TST.Translate 0 -10, TST.Scale 0.8 0.8 ]
--                    ]
--                    [ text <| "KO=" ++ String.fromInt ko ]
--
--            Patrolling _ _ ->
--                text ""
--        ]


viewSelectionOutline isSelected =
    viewBool isSelected (square cellWidthPx [ stroke "hsl(26, 91%, 47%)", SA.rx "10" ])


noFill =
    fill "none"


group =
    Svg.g


viewHoverPath : Cons IntPos -> List HM
viewHoverPath =
    Cons.toList >> List.map (viewPathPos "hsl(209, 100%, 79%)")


viewGuardPath : Cons IntPos -> List HM
viewGuardPath =
    Cons.toList >> List.map (viewPathPos "hsl(324, 100%, 75%)")


viewPathPos c pos =
    circle (innerCellWidth * 0.2)
        [ svgCellTransform pos
        , SA.fill "none"
        , SA.stroke c
        ]


viewWall : IntPos -> HM
viewWall pos =
    square innerCellWidth
        [ SA.fill "hsl(0, 25%, 65%)"
        , SA.rx "10"
        , svgCellTransform pos
        ]


viewBackgroundTile pos =
    square innerCellWidth
        [ SA.fill "none"
        , SA.stroke "hsl(209, 100%, 79%)"
        , SA.rx "10"
        , svgCellTransform pos
        ]


svgCellTransform ( x, y ) =
    TSA.transform
        [ TST.Translate
            (toFloat x * cellWidthPx + cellWidthPx * 0.5 - gridWidthPx * 0.5)
            (toFloat y * cellWidthPx + cellWidthPx * 0.5 - gridHeightPx * 0.5)
        ]


square w =
    rect w w


circle r xs =
    Svg.circle (Px.r r :: xs) []


rect w h xs =
    Svg.rect
        (Px.x (w * -0.5)
            :: Px.y (h * -0.5)
            :: Px.width w
            :: Px.height h
            :: xs
        )
        []


px i =
    String.fromInt i ++ "px"


wpx =
    px >> style "width"



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
