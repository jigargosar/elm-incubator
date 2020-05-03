module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import List.Extra



-- Grid


type alias Grid =
    List (List Int)


emptyGrid : Grid
emptyGrid =
    [ [ 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


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
mapGridRows =
    List.map


mapGridColumns : (List Int -> List Int) -> Grid -> Grid
mapGridColumns fun grid =
    grid
        |> transposeGrid
        |> List.map fun
        |> transposeGrid


gridPositions : List ( Int, Int )
gridPositions =
    mapRangeLen 4 (\r -> mapRangeLen 4 (Tuple.pair r))
        |> List.concat


mapRangeLen len func =
    List.range 0 (len - 1) |> List.map func


transposeGrid : Grid -> List (List Int)
transposeGrid grid0 =
    List.foldl
        (\( r, c ) grid ->
            gridSetAt c r (gridGetAt r c grid0) grid
        )
        grid0
        gridPositions


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


gridSetAt : Int -> Int -> Int -> Grid -> Grid
gridSetAt r c v =
    List.Extra.updateAt r (List.Extra.setAt c v)


gridGetAt : Int -> Int -> Grid -> Int
gridGetAt r c =
    List.Extra.getAt r
        >> Maybe.andThen (List.Extra.getAt c)
        >> Maybe.withDefault 0


viewGrid : Grid -> HM
viewGrid grid =
    let
        rows =
            grid

        viewRow row =
            div [ class "flex" ] (List.map viewCell row)

        viewCell num =
            div [ class "w3 tc" ] [ text (String.fromInt num) ]
    in
    div [ class "flex flex-column code f1" ] (List.map viewRow rows)



-- Model


type alias Model =
    {}


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( {}
    , Cmd.none
    )



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


type alias DM =
    Document Msg


grid1 : Grid
grid1 =
    [ [ 2, 0, 0, 0 ]
    , [ 2, 4, 4, 4 ]
    , [ 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


viewGridWithOps : List GridOp -> Grid -> HM
viewGridWithOps ops grid =
    List.foldl
        (\op ( vl, g ) ->
            let
                ng =
                    updateGrid op g
            in
            ( viewNamedGrid (Debug.toString op) ng :: vl, ng )
        )
        ( [ viewNamedGrid "Initial Grid" grid ], grid )
        ops
        |> (Tuple.first >> List.reverse)
        |> div []


viewNamedGrid name grid =
    div [ class "pl5 pv3" ]
        [ div [ class "f3 pa2 " ] [ text name ]
        , viewGrid grid
        ]


view : Model -> DM
view _ =
    Document "2048"
        [ div [ class "f3 pa3" ] [ text "2048 grid" ]
        , viewGridWithOps [ SlideDown, SlideUp ] grid1
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
