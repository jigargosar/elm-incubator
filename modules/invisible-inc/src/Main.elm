port module Main exposing (main)

import AStar
import Browser exposing (Document)
import Browser.Events
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
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
import Styles
import Task
import XY exposing (XY)


port cache : String -> Cmd msg



-- IntPos


type alias IntPos =
    ( Int, Int )


isMemberOfSize : IntSize -> IntPos -> Bool
isMemberOfSize s p =
    sizeContains p s



-- IntSize


type alias IntSize =
    { width : Int
    , height : Int
    }


sizeContains : IntPos -> IntSize -> Bool
sizeContains ( x, y ) s =
    (clamp 0 (s.width - 1) x == x)
        && (clamp 0 (s.height - 1) y == y)


positionsOf : IntSize -> List IntPos
positionsOf s =
    List.range 0 (s.width - 1)
        |> List.concatMap (\x -> List.range 0 (s.height - 1) |> List.map (pair x))



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


adjacentOf pos =
    [ mapFirst (add 1)
    , mapFirst (add -1)
    , mapSecond (add 1)
    , mapSecond (add -1)
    ]
        |> List.map (applyTo pos)


initGuard : Set IntPos -> Guard
initGuard walls =
    let
        startPos =
            ( 8, 12 )

        endPos =
            ( 0, 0 )

        p2 =
            AStar.findPath AStar.pythagoreanCost (mvf walls) startPos endPos
                |> Maybe.andThen (prepend startPos >> List.take 10 >> LZ.fromList)
                |> Maybe.withDefault (LZ.singleton startPos)
    in
    { path = p2
    }


mvf walls pos =
    adjacentOf pos
        |> Set.fromList
        |> Set.filter (isMemberOfSize gridSize)
        |> setRemoveAll walls


editGuard : (IntPos -> Set IntPos) -> IntPos -> IntPos -> Guard -> Guard
editGuard mv startPos endPos guard =
    { guard
        | path =
            AStar.findPath AStar.pythagoreanCost mv startPos endPos
                |> Maybe.andThen (prepend startPos >> List.take 10 >> LZ.fromList)
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
    JD.succeed
        (\walls ->
            initModel walls
                |> mapWalls (always walls)
        )
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
    { guard = initGuard walls
    , agent = ( 5, 5 )
    , status = PlayerTurn
    , walls = walls
    , edit = EditWall
    , selection = AgentSelected
    , hover = ( 0, 0 )
    }


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
            if sizeContains pos gridSize then
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
                    |> mapGuard (guardSetStartPosition (mvf model.walls) pos)
                    |> setEdit EditGuardDest

            EditGuardDest ->
                model
                    |> mapGuard (guardSetEndPosition (mvf model.walls) pos)
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
            , Styles.wi gridWidthPx
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
        [ Styles.wi gridWidthPx
        , Styles.hi gridHeightPx
        , ME.click (JD.map GridPosClicked mouseGridPosDecoder)
        , ME.over (JD.map GridPosHovered mouseGridPosDecoder)
        ]
        ((positionsOf gridSize
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
                        viewHoverPath (toHoverPath (mvf model.walls) model.agent model.hover)

                    GuardSelected ->
                        []
               )
        )


toHoverPath : (IntPos -> Set IntPos) -> IntPos -> IntPos -> List IntPos
toHoverPath mv startPos endPos =
    AStar.findPath AStar.pythagoreanCost mv startPos endPos
        |> Maybe.map (prepend startPos >> List.take 10)
        |> Maybe.withDefault (List.singleton startPos)


viewAgent isSelected pos =
    div
        [ Styles.wi cellWidthPx
        , Styles.hi cellWidthPx
        , style "transform" (renderCellTransform pos)
        , style "padding" "3px"
        , class "absolute"
        ]
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
    style "box-shadow"
        ([ "0 0 0 2px hsl(0, 0%, 100%)"
         , "0 0 0 4px hsl(14, 100%, 57%)"
         ]
            |> String.join ","
        )


viewGuard : Bool -> Guard -> HM
viewGuard isSelected guard =
    let
        pos =
            LZ.current guard.path
    in
    div
        [ Styles.wi cellWidthPx
        , Styles.hi cellWidthPx
        , style "transform" (renderCellTransform pos)
        , style "padding" "3px"
        , class "absolute"
        ]
        [ div
            [ class "w-100 h-100 br3 bg-pink"
            , if isSelected then
                shadowSelection

              else
                class ""
            ]
            []
        ]


viewHoverPath : List IntPos -> List HM
viewHoverPath ps =
    List.map viewHoverPathPos ps


viewGuardPath : Guard -> List HM
viewGuardPath guard =
    let
        ps =
            LZ.toList guard.path
    in
    List.map viewGuardPathPos ps


viewGuardPathPos pos =
    div
        [ Styles.wi cellWidthPx
        , Styles.hi cellWidthPx
        , style "transform" (renderCellTransform pos)
        , style "padding" "3px"
        , class "absolute"
        ]
        [ div [ class "w-100 h-100 flex items-center justify-center" ]
            [ div
                [ class "br-pill bg-pink ba red"
                , Styles.wi 15
                , Styles.hi 15
                ]
                []
            ]
        ]


viewHoverPathPos pos =
    div
        [ Styles.wi cellWidthPx
        , Styles.hi cellWidthPx
        , style "transform" (renderCellTransform pos)
        , style "padding" "3px"
        , class "absolute"
        ]
        [ div [ class "w-100 h-100 flex items-center justify-center" ]
            [ div
                [ class "br-pill ba b--red bg-light-blue "
                , Styles.wi 15
                , Styles.hi 15
                ]
                []
            ]
        ]


viewWall : IntPos -> HM
viewWall pos =
    div
        [ Styles.wi cellWidthPx
        , Styles.hi cellWidthPx
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
        [ Styles.wi cellWidthPx
        , Styles.hi cellWidthPx
        , style "transform" (renderCellTransform pos)
        , style "padding" "3px"
        , class "absolute"
        ]
        [ div [ class "w-100 h-100 br3 ba bw1 b--light-blue" ] [] ]


renderCellTransform ( x, y ) =
    "translate(" ++ String.fromInt (x * cellWidthPx) ++ "px, " ++ String.fromInt (y * cellWidthPx) ++ "px)"



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
