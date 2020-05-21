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
import ListZipper as LZ exposing (ListZipper)
import More exposing (..)
import MouseEvents as ME
import Process
import Set exposing (Set)
import Task
import XY exposing (XY)


port cache : String -> Cmd msg



-- IntPos


isMemberOfSize : IntSize -> IntPos -> Bool
isMemberOfSize s p =
    IntSize.member p s



-- Guard


type alias Guard =
    { path : ListZipper IntPos
    }


positionOfGuard : Guard -> IntPos
positionOfGuard =
    .path >> LZ.current


targetPositionOfGuard : Guard -> IntPos
targetPositionOfGuard =
    .path >> LZ.last >> LZ.current


initGuard : IntPos -> Guard
initGuard pos =
    { path = LZ.singleton pos
    }


editGuard : (IntPos -> Set IntPos) -> IntPos -> IntPos -> Guard -> Guard
editGuard mv startPos endPos guard =
    { guard
        | path =
            AStar.findPath AStar.pythagoreanCost mv startPos endPos
                |> Maybe.andThen (cons startPos >> List.take 10 >> LZ.fromList)
                |> Maybe.withDefault (LZ.singleton startPos)
    }


guardSetStartPosition : (IntPos -> Set IntPos) -> IntPos -> Guard -> Guard
guardSetStartPosition mv startPos guard =
    editGuard mv startPos (targetPositionOfGuard guard) guard


guardSetEndPosition : (IntPos -> Set IntPos) -> IntPos -> Guard -> Guard
guardSetEndPosition mv endPos guard =
    editGuard mv (positionOfGuard guard) endPos guard


stepGuard : Guard -> ( Bool, Guard )
stepGuard guard =
    case LZ.right guard.path of
        Just path ->
            ( False, { guard | path = path } )

        Nothing ->
            ( True, { guard | path = LZ.swap guard.path } )



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
    , selection : Selection
    , hover : IntPos
    }


occupied : Model -> Set IntPos
occupied model =
    model.walls
        |> Set.insert (positionOfGuard model.guard)
        |> Set.insert model.agent


unOccupiedNeighbours : Model -> IntPos -> Set IntPos
unOccupiedNeighbours =
    occupied >> unOccupiedNeighboursHelp


unOccupiedNeighboursHelp : Set IntPos -> IntPos -> Set IntPos
unOccupiedNeighboursHelp blocked pos =
    IntSize.adjacentMembers pos gridSize
        |> Set.fromList
        |> setRemoveAll blocked


type Selection
    = AgentSelected
    | GuardSelected


type Status
    = PlayerTurn
    | EnemyTurn


type Edit
    = EditWall
    | EditGuard
    | EditGuardDest


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

        model =
            { guard = initGuard guardPos
            , agent = agentPos
            , status = PlayerTurn
            , walls = filteredWalls
            , edit = EditWall
            , selection = AgentSelected
            , hover = ( 0, 0 )
            }
    in
    model
        |> mapGuard (guardSetEndPosition (unOccupiedNeighbours model) ( 5, 12 ))


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
    | GridPosHovered IntPos


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        SetEdit edit ->
            ( { model | edit = edit }, Cmd.none )

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

        GridPosClicked pos ->
            if IntSize.member pos gridSize then
                updateOnPosClicked pos model
                    |> withEffect (cacheIfChanged model)

            else
                ( model, Cmd.none )

        GridPosHovered hover ->
            ( { model | hover = hover }, Cmd.none )


updateOnPosClicked : IntPos -> Model -> Model
updateOnPosClicked pos model =
    if positionOfGuard model.guard == pos then
        { model | selection = GuardSelected }

    else if model.agent == pos then
        { model | selection = AgentSelected }

    else
        case model.edit of
            EditWall ->
                if pos /= positionOfGuard model.guard then
                    model
                        |> mapWalls (toggleSetMember pos)

                else
                    model

            EditGuard ->
                model
                    |> mapGuard (guardSetStartPosition (unOccupiedNeighbours model) pos)
                    |> setEdit EditGuardDest

            EditGuardDest ->
                model
                    |> mapGuard (guardSetEndPosition (unOccupiedNeighbours model) pos)
                    |> setEdit EditGuard


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


viewGrid : Model -> HM
viewGrid model =
    div
        [ wpx gridWidthPx
        , hpx gridHeightPx
        , ME.click (JD.map GridPosClicked mouseGridPosDecoder)
        , ME.over (JD.map GridPosHovered mouseGridPosDecoder)
        ]
        ((IntSize.positions gridSize
            |> List.map viewBackgroundTile
         )
            ++ (model.walls |> Set.toList |> List.map viewWall)
            ++ [ viewAgent
                    (case model.selection of
                        AgentSelected ->
                            True

                        GuardSelected ->
                            False
                    )
                    model.agent
               , viewGuard
                    (case model.selection of
                        AgentSelected ->
                            False

                        GuardSelected ->
                            True
                    )
                    model.guard
               ]
            ++ viewGuardPath model.guard
            ++ (case model.selection of
                    AgentSelected ->
                        viewHoverPath (toHoverPath (unOccupiedNeighbours model) model.agent model.hover)

                    GuardSelected ->
                        []
               )
        )


toHoverPath : (IntPos -> Set IntPos) -> IntPos -> IntPos -> Cons IntPos
toHoverPath mv startPos endPos =
    AStar.findPath AStar.pythagoreanCost mv startPos endPos
        |> Maybe.withDefault []
        |> List.take 10
        |> Cons.init startPos


viewAgent isSelected pos =
    div
        (cellContainerStyles pos)
        [ div
            [ class "w-100 h-100 br3 bg-light-blue"
            , if isSelected then
                shadowSelection

              else
                class ""
            ]
            []
        ]


shadowSelection =
    shadows
        [ "0 0 0 2px hsl(0, 0%, 100%)"
        , "0 0 0 4px hsl(14, 100%, 57%)"
        ]


viewGuard : Bool -> Guard -> HM
viewGuard isSelected guard =
    let
        pos =
            LZ.current guard.path
    in
    div
        (cellContainerStyles pos)
        [ div
            [ class "w-100 h-100 br3 bg-pink"
            , if isSelected then
                shadowSelection

              else
                class ""
            ]
            []
        ]


viewHoverPath : Cons IntPos -> List HM
viewHoverPath =
    Cons.toList >> List.map (viewPathPos [ class "b--light-blue" ])


viewPathPos attrs pos =
    div
        (cellContainerStyles pos)
        [ div
            ([ class "br-pill ba bg-white"
             , class "o-60"
             , bwpx 3
             , wpx 15
             , hpx 15
             , shadows [ "#fff 0px 0px 0px 2px" ]
             ]
                ++ attrs
            )
            []
        ]


viewGuardPath : Guard -> List HM
viewGuardPath guard =
    let
        ps =
            LZ.toList guard.path
    in
    List.map (viewPathPos [ class "b--pink" ]) ps


viewWall : IntPos -> HM
viewWall pos =
    div
        (cellContainerStyles pos)
        [ div
            [ class "w-100 h-100 br3 o-70"
            , style "background-color" "hsl(0, 25%, 65%)"
            ]
            []
        ]


cellContainerStyles pos =
    [ wpx cellWidthPx
    , hpx cellWidthPx
    , cellTransformStyle pos
    , pa 3
    , absolute
    , class "flex items-center justify-center"
    ]


cellTransformStyle pos =
    style "transform" (renderCellTransform pos)


viewBackgroundTile pos =
    div
        (cellContainerStyles pos)
        [ div [ class "w-100 h-100 br3 ba bw1 b--light-blue" ] [] ]


renderCellTransform ( x, y ) =
    "translate(" ++ px (x * cellWidthPx) ++ ", " ++ px (y * cellWidthPx) ++ ")"



-- STYLES


absolute =
    class "absolute"


pa =
    px >> style "padding"


px i =
    String.fromInt i ++ "px"


wpx =
    px >> style "width"


hpx =
    px >> style "height"



--noinspection SpellCheckingInspection


bwpx =
    px >> style "border-width"


shadows =
    join ", " >> style "box-shadow"



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
