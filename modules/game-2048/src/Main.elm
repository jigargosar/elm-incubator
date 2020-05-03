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



-- Board


type alias Board =
    { seed : Random.Seed
    , grid : Grid
    , lastGen : Maybe ( Int, Int )
    }


initBoard : Random.Seed -> Grid.Lists Int -> Board
initBoard seed lists =
    { seed = seed
    , grid = Grid.fromLists { width = 4, height = 4 } 0 lists
    , lastGen = Nothing
    }


updateBoard : SlideMsg -> Board -> Board
updateBoard gridOp board =
    let
        nextGrid : Grid
        nextGrid =
            updateGrid gridOp board.grid
    in
    if nextGrid == board.grid then
        board

    else
        case gridEmptyPositions nextGrid of
            [] ->
                board

            h :: t ->
                let
                    ( ( pos, randomVal ), nextSeed ) =
                        Random.step
                            (Random.pair
                                (Random.uniform h t)
                                (Random.uniform 2 [ 4 ])
                            )
                            board.seed
                in
                { grid = Grid.set pos randomVal nextGrid |> Maybe.withDefault nextGrid
                , lastGen = Just pos
                , seed = nextSeed
                }


viewBoard : Board -> HM
viewBoard board =
    let
        rows =
            Grid.toLists board.grid

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
            if Just ( ri, ci ) == board.lastGen then
                cellContainer [ text (numToString num) ]

            else
                cellContainer [ text (numToString num) ]
    in
    div [ class "flex flex-column bt f4 b--red" ] (List.indexedMap viewRow rows)



-- Grid


type alias Grid =
    Grid.Grid Int


gridEmptyPositions : Grid -> List Grid.Pos
gridEmptyPositions grid =
    Grid.toDict grid
        |> Dict.filter (\_ v -> v == 0)
        |> Dict.keys


type SlideMsg
    = SlideUp
    | SlideDown
    | SlideLeft
    | SlideRight


updateGrid : SlideMsg -> Grid -> Grid
updateGrid gridOp =
    case gridOp of
        SlideUp ->
            Grid.mapColumnLists (List.reverse >> compactNumRight >> List.reverse)

        SlideDown ->
            Grid.mapColumnLists compactNumRight

        SlideLeft ->
            Grid.mapRowLists (List.reverse >> compactNumRight >> List.reverse)

        SlideRight ->
            Grid.mapRowLists compactNumRight


eq =
    (==)


compactNumRight : List Int -> List Int
compactNumRight =
    let
        padLeft : List Int -> List Int
        padLeft l =
            List.repeat (4 - List.length l) 0 ++ l

        func v acc =
            case acc of
                h :: t ->
                    if v == h then
                        v + h :: t

                    else
                        v :: acc

                _ ->
                    v :: acc
    in
    List.filter (eq 0 >> not)
        >> List.foldr func []
        >> padLeft



-- Model


type alias Model =
    { list : List NamedGrid }


type alias Flags =
    ()


type alias NamedGrid =
    ( String, Board )


initialNamedGridList : List NamedGrid
initialNamedGridList =
    toNamedGridList
        [--SlideDown
         --, SlideUp
         --, SlideLeft
         --, SlideDown
        ]
        (initBoard (Random.initialSeed 0)
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


updateGridListWithOp : SlideMsg -> Model -> Model
updateGridListWithOp gridOp model =
    case model.list of
        [] ->
            model

        ( _, g2 ) :: _ ->
            { model
                | list =
                    ( Debug.toString gridOp, updateBoard gridOp g2 ) :: model.list
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


toNamedGridList : List SlideMsg -> Board -> List ( String, Board )
toNamedGridList ops board =
    List.foldl
        (\op ( stack, g2 ) ->
            let
                ng2 =
                    updateBoard op g2
            in
            ( ( Debug.toString op, ng2 ) :: stack, ng2 )
        )
        ( [ ( "Initial Grid", board ) ], board )
        ops
        |> Tuple.first


viewNamedGrid name grid =
    div [ class "pa3 pv2" ]
        [ div [ class "f4 pa2 " ] [ text name ]
        , viewBoard grid
        ]


viewNamedGridList : List ( String, Board ) -> HM
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
