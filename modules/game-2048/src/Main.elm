module Main exposing (main)

import Basics.Extra exposing (uncurry)
import Browser exposing (Document)
import Browser.Events
import Dict exposing (Dict)
import Grid
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as D
import Random



-- GridWithRandomSeed


type alias Grid2 =
    { seed : Random.Seed
    , grid : Grid
    , lastGen : Maybe ( Int, Int )
    }


initGrid2 : Random.Seed -> List (List Int) -> Grid2
initGrid2 seed lists =
    { seed = seed
    , grid = gridFromLists lists
    , lastGen = Nothing
    }


updateGrid2 : GridOp -> Grid2 -> Grid2
updateGrid2 gridOp grid2 =
    let
        nextGrid : Grid
        nextGrid =
            updateGrid gridOp grid2.grid
    in
    if nextGrid == grid2.grid then
        grid2

    else
        case gridEmptyPositions nextGrid of
            [] ->
                grid2

            h :: t ->
                let
                    ( ( pos, randomVal ), nextSeed ) =
                        Random.step
                            (Random.pair
                                (Random.uniform h t)
                                (Random.uniform 2 [ 4 ])
                            )
                            grid2.seed
                in
                { grid = Grid.set pos randomVal nextGrid |> Maybe.withDefault nextGrid
                , lastGen = Just pos
                , seed = nextSeed
                }


viewGrid2 : Grid2 -> HM
viewGrid2 grid2 =
    let
        rows =
            Grid.toLists grid2.grid

        viewRow ri row =
            div [ class "flex br bb b--inherit" ] (List.indexedMap (viewCell ri) row)

        numToString num =
            case num of
                0 ->
                    ""

                _ ->
                    String.fromInt num
                        --|> always "2048"
                        |> identity

        cellContainer children =
            div [ class "bl  b--inherit w3 h2 flex items-center justify-center" ]
                children

        viewCell ri ci num =
            if Just ( ri, ci ) == grid2.lastGen then
                cellContainer [ text (numToString num) ]

            else
                cellContainer [ text (numToString num) ]
    in
    div [ class "flex flex-column bt f4 b--red" ] (List.indexedMap viewRow rows)



-- Grid


type alias Grid =
    Grid.Grid Int


gridFromLists : List (List Int) -> Grid
gridFromLists =
    Grid.fromLists { width = 4, height = 4 } 0


gridEmptyPositions : Grid -> List Grid.Pos
gridEmptyPositions grid =
    Grid.toDict grid
        |> Dict.filter (\_ v -> v == 0)
        |> Dict.keys


type GridOp
    = SlideUp
    | SlideDown
    | SlideLeft
    | SlideRight


updateGrid : GridOp -> Grid -> Grid
updateGrid gridOp =
    case gridOp of
        SlideUp ->
            up

        SlideDown ->
            down

        SlideLeft ->
            left

        SlideRight ->
            right


down =
    mapGridColumns gridListSlideRight


up =
    mapGridColumns gridListSlideLeft


left =
    mapGridRows gridListSlideLeft


right =
    mapGridRows gridListSlideRight


mapGridRows : (List Int -> List Int) -> Grid -> Grid
mapGridRows fun grid =
    grid
        |> Grid.toLists
        |> List.map fun
        |> Grid.fromLists { width = 4, height = 4 } 0


mapGridColumns : (List Int -> List Int) -> Grid -> Grid
mapGridColumns fun grid =
    grid
        |> Grid.transpose
        |> Grid.toLists
        |> List.map fun
        |> Grid.fromLists { width = 4, height = 4 } 0
        |> Grid.transpose


gridListSlideRight : List Int -> List Int
gridListSlideRight row =
    let
        compacted =
            gridCompactRow row
    in
    gridRowPadding compacted ++ compacted


gridListSlideLeft : List Int -> List Int
gridListSlideLeft =
    List.reverse >> gridListSlideRight >> List.reverse


eq =
    (==)


gridCompactRow : List Int -> List Int
gridCompactRow row =
    row
        |> List.filter (eq 0 >> not)
        |> List.foldr
            (\v acc ->
                case acc of
                    h :: t ->
                        if v == h then
                            v + h :: t

                        else
                            v :: acc

                    _ ->
                        v :: acc
            )
            []


gridRowPadding : List Int -> List Int
gridRowPadding row =
    let
        padLength =
            4 - List.length row
    in
    List.repeat padLength 0



-- Model


type alias Model =
    { list : List NamedGrid }


type alias Flags =
    ()


type alias NamedGrid =
    ( String, Grid2 )


initialNamedGridList : List NamedGrid
initialNamedGridList =
    toNamedGridList
        [--SlideDown
         --, SlideUp
         --, SlideLeft
         --, SlideDown
        ]
        (initGrid2 (Random.initialSeed 0)
            [ [ 2, 0, 0, 0 ]
            , [ 2, 4, 4, 4 ]
            , [ 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0 ]
            ]
        )


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { list = initialNamedGridList }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | OnKeyDown String


updateGridListWithOp : GridOp -> Model -> Model
updateGridListWithOp gridOp model =
    case model.list of
        [] ->
            model

        ( _, g2 ) :: _ ->
            { model
                | list =
                    ( Debug.toString gridOp, updateGrid2 gridOp g2 ) :: model.list
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        OnKeyDown string ->
            let
                maybeOp =
                    case string of
                        "ArrowUp" ->
                            Just SlideUp

                        "ArrowDown" ->
                            Just SlideDown

                        "ArrowLeft" ->
                            Just SlideLeft

                        "ArrowRight" ->
                            Just SlideRight

                        _ ->
                            Nothing
            in
            case maybeOp of
                Just gridOp ->
                    ( updateGridListWithOp gridOp model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown
            (D.field "key" D.string |> D.map OnKeyDown)
        ]



-- View


type alias DM =
    Document Msg


toNamedGridList : List GridOp -> Grid2 -> List ( String, Grid2 )
toNamedGridList ops grid2 =
    List.foldl
        (\op ( stack, g2 ) ->
            let
                ng2 =
                    updateGrid2 op g2
            in
            ( ( Debug.toString op, ng2 ) :: stack, ng2 )
        )
        ( [ ( "Initial Grid", grid2 ) ], grid2 )
        ops
        |> Tuple.first


viewNamedGrid name grid =
    div [ class "pa3 pv2" ]
        [ div [ class "f4 pa2 " ] [ text name ]
        , viewGrid2 grid
        ]


viewNamedGridList : List ( String, Grid2 ) -> HM
viewNamedGridList =
    List.map (uncurry viewNamedGrid)
        >> div [ class "flex flex-column items-center" ]


view : Model -> DM
view model =
    Document "2048"
        [ div [ class "f3 pa3" ] [ text "2048 grid" ]
        , viewNamedGridList (List.take 4 model.list)
        ]


type alias HM =
    Html Msg



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
