module GameRunner exposing (main)

import Basics.Extra exposing (uncurry)
import Browser exposing (Document)
import GameModel as Game
import Grid exposing (GI)
import Html exposing (Html, button, div, node, table, text)
import Html.Attributes exposing (autofocus, class, style)
import Html.Events exposing (onClick)
import List.Extra
import PointerEvents as PE


updateSelection : GI -> Bool -> Game.Model -> Maybe Game.Model
updateSelection idx wasSelected game =
    let
        stack =
            Game.info game
                |> .selectionStack
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



-- Model


type Model
    = Selecting Game.Model
    | Over Game.Info


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( Selecting Game.init
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | PlayAnother
    | CollectSelection
    | ToggleSelection GI Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        PlayAnother ->
            case model of
                Selecting _ ->
                    ( model, Cmd.none )

                Over _ ->
                    init ()

        ToggleSelection idx wasSelected ->
            case model of
                Selecting game ->
                    let
                        nm =
                            Selecting (updateSelection idx wasSelected game |> Maybe.withDefault game)
                    in
                    ( nm, Cmd.none )

                Over _ ->
                    ( model, Cmd.none )

        CollectSelection ->
            case model of
                Selecting game ->
                    ( case Game.makeMove game of
                        Game.InvalidMove ->
                            model

                        Game.NextState ng ->
                            Selecting ng

                        Game.GameOver info ->
                            Over info
                    , Cmd.none
                    )

                Over _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


type alias DM =
    Document Msg


view : Model -> DM
view model =
    Document "GameRunner"
        (node "style" [] [ text """
            body {
                user-select:none;

            }
        """ ]
            :: (case model of
                    Selecting game ->
                        [ div [ class "pa3" ] [ text "Game Running" ]
                        , viewGameInfo (Game.info game)
                        , div [ class "pa3" ]
                            [ btn CollectSelection "collect"
                            ]
                        ]

                    Over info ->
                        [ div [ class "pa3" ] [ text "Game Over" ]
                        , viewGameInfo info
                        , div [ class "pa3" ] [ btn PlayAnother "Play Again?" ]
                        ]
               )
        )


type alias HM =
    Html Msg


viewGameInfo : Game.Info -> HM
viewGameInfo i =
    div []
        [ div [ class "pa3" ]
            [ text
                (Debug.toString
                    { movesLeft = i.movesLeft
                    , targetSeeds = i.targetSeeds
                    , targetWater = i.targetWater
                    }
                )
            ]
        , viewGameTable i
        ]


viewGameTable : Game.Info -> HM
viewGameTable info =
    let
        toCellConfig idx =
            let
                selIdx =
                    List.reverse info.selectionStack |> List.Extra.elemIndex idx

                isSelected =
                    selIdx /= Nothing

                selectionMsg =
                    ToggleSelection idx isSelected
            in
            { selectionIdx = selIdx
            , selectionMsg = selectionMsg
            , gridIdx = idx
            }

        viewCell : GI -> Game.Cell -> HM
        viewCell idx c =
            viewCellWithConfig (toCellConfig idx) c
    in
    viewGridTable viewCell info.grid


viewCellWithConfig :
    { a
        | selectionIdx : Maybe Int
        , selectionMsg : Msg
    }
    -> Game.Cell
    -> HM
viewCellWithConfig conf cell =
    Html.td []
        [ div
            [ PE.onPrimaryEnterAndDown conf.selectionMsg
            , PE.onPrimaryDown conf.selectionMsg
            , class "br3 w3 h3 flex"
            , style "transition" "transform 500ms"
            , style "transform"
                (if conf.selectionIdx == Nothing then
                    "scale(1.0)"

                 else
                    "scale(0.8)"
                )
            , class
                (case cell of
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
            [ text
                (conf.selectionIdx
                    |> Maybe.map String.fromInt
                    |> Maybe.withDefault ""
                )
            ]
        ]


viewGridTable : (GI -> a -> HM) -> Grid.Grid a -> HM
viewGridTable renderCell grid =
    let
        cells =
            Grid.toList grid

        rows =
            List.Extra.gatherEqualsBy (Tuple.first >> Tuple.second) cells
                |> List.map (uncurry (::))

        renderCell_ ( gi, a ) =
            Html.td [] [ renderCell gi a ]

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
    table [ class "pa3" ]
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
