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


gridSetAt : Int -> Int -> Int -> Grid -> Grid
gridSetAt r c v =
    List.Extra.updateAt r (List.Extra.setAt c v)


gridGetAt : Int -> Int -> Grid -> Int
gridGetAt r c grid =
    List.Extra.getAt r grid
        |> Maybe.andThen (List.Extra.getAt c)
        |> Maybe.withDefault 0


gridPositions : List ( Int, Int )
gridPositions =
    List.range 0 3
        |> List.concatMap (\r -> List.range 0 3 |> List.map (Tuple.pair r))


gridEmptyPositions : Grid -> List ( Int, Int )
gridEmptyPositions grid =
    List.filter (\( r, c ) -> gridGetAt r c grid == 0) gridPositions


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
