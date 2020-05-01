module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import List.Extra
import Random



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


initialGrid : Grid
initialGrid =
    emptyGrid
        |> gridSetAt 0 0 2
        |> gridSetAt 1 1 4
        |> gridSetAt 1 2 4
        |> gridSetAt 1 3 4
        |> mapGridRows gridRowSlideLeft
        |> mapGridRows gridRowSlideRight
        |> identity


mapGridRows : (List Int -> List Int) -> Grid -> Grid
mapGridRows =
    List.map


mapGridColumns : (List Int -> List Int) -> Grid -> Grid
mapGridColumns fun grid =
    grid
        |> gridToColumns
        |> List.map fun
        |> gridFromColumns


gridToColumns : Grid -> List (List Int)
gridToColumns grid =
    grid


gridFromColumns : Grid -> List (List Int)
gridFromColumns grid =
    grid


gridRowSlideRight : List Int -> List Int
gridRowSlideRight row =
    let
        compacted =
            gridCompactRow row
    in
    gridRowPadding compacted ++ compacted


gridRowSlideLeft : List Int -> List Int
gridRowSlideLeft =
    List.reverse >> gridRowSlideRight >> List.reverse


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
    { grid : Grid }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { grid = initialGrid }
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


view : Model -> DM
view model =
    Document "2048"
        [ div [ class "f3 pa3" ] [ text "2048 grid" ]
        , div [ class "pl5 pv3 " ] [ viewGrid model.grid ]
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
