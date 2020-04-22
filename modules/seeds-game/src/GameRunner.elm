module GameRunner exposing (main)

import AbstractGame as G
import Browser exposing (Document)
import Grid exposing (GI)
import Html exposing (Html, button, div, node, table, text)
import Html.Attributes exposing (autofocus, class, style)
import Html.Events exposing (onClick)
import List.Extra
import PointerEvents as PE


updateSelection idx wasSelected game =
    let
        stack =
            G.info game
                |> .selectionStack
    in
    if wasSelected && List.member idx stack then
        -- Remove
        case stack of
            only :: [] ->
                if only == idx then
                    G.pop game

                else
                    Nothing

            _ :: secondLast :: _ ->
                if secondLast == idx then
                    G.pop game

                else
                    Nothing

            _ ->
                Nothing

    else if not wasSelected && not (List.member idx stack) then
        -- Add
        G.push idx game

    else
        -- NoOp
        Nothing



-- Model


type Model
    = Running G.GameModel
    | Over G.Info
    | Won G.Info


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( Running G.initGame
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | PlayAnother
    | Collect
    | ToggleSelection GI Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        PlayAnother ->
            case model of
                Running _ ->
                    ( model, Cmd.none )

                Over _ ->
                    init ()

                Won _ ->
                    init ()

        ToggleSelection idx wasSelected ->
            case model of
                Running game ->
                    let
                        nm =
                            Running (updateSelection idx wasSelected game |> Maybe.withDefault game)
                    in
                    ( nm, Cmd.none )

                Over _ ->
                    ( model, Cmd.none )

                Won _ ->
                    ( model, Cmd.none )

        Collect ->
            case model of
                Running moveBuilder ->
                    let
                        nm =
                            case G.makeMove moveBuilder of
                                G.InvalidMove ->
                                    model

                                G.NextState ng ->
                                    Running ng

                                G.GameLost info ->
                                    Over info

                                G.GameWon info ->
                                    Won info
                    in
                    ( nm, Cmd.none )

                Over _ ->
                    ( model, Cmd.none )

                Won _ ->
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
                    Running moveBuilder ->
                        [ div [ class "pa3" ] [ text "Game Running" ]
                        , viewGameInfo (G.info moveBuilder)
                        , div [ class "pa3" ]
                            [ btn
                                Collect
                                "collect"
                            ]
                        ]

                    Over info ->
                        [ div [ class "pa3" ] [ text "Game Lost" ]
                        , viewGameInfo info
                        , div [ class "pa3" ] [ btn PlayAnother "Play Again?" ]
                        ]

                    Won info ->
                        [ div [ class "pa3" ] [ text "Game Won" ]
                        , viewGameInfo info
                        , div [ class "pa3" ] [ btn PlayAnother "Play Again?" ]
                        ]
               )
        )


type alias HM =
    Html Msg


viewGameInfo : G.Info -> HM
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
        , viewGameCells i
        ]


viewGameCells : G.Info -> HM
viewGameCells info =
    let
        cells =
            Grid.toList info.grid

        rows =
            List.Extra.gatherEqualsBy (Tuple.first >> Tuple.second) cells

        viewRow : Int -> ( ( GI, G.Cell ), List ( GI, G.Cell ) ) -> HM
        viewRow y ( h, t ) =
            Html.tr []
                (styledTH [ text "y", text (String.fromInt y) ]
                    :: List.map (viewCell info) (h :: t)
                )

        styledTH =
            Html.th [ class "code f4 pa1" ]

        viewHeadCell x _ =
            styledTH [ text "x", text (String.fromInt x) ]

        viewTHead mh =
            case mh of
                Just ( h, t ) ->
                    Html.thead []
                        (styledTH [ text "x,y" ]
                            :: List.indexedMap viewHeadCell (h :: t)
                        )

                Nothing ->
                    text ""
    in
    table [ class "pa3" ]
        (viewTHead (List.head rows) :: List.indexedMap viewRow rows)


computeScale max n =
    let
        step =
            (hi - lo) / toFloat max

        lo =
            0.5

        hi =
            0.8
    in
    (toFloat n * step) + lo


viewCell : G.Info -> ( GI, G.Cell ) -> HM
viewCell info ( ( _, _ ) as idx, c ) =
    let
        selIdx =
            List.reverse info.selectionStack |> List.Extra.elemIndex idx

        scaleForSelection =
            info.selectionStack
                |> List.Extra.elemIndex idx
                |> Maybe.map (computeScale (List.length info.selectionStack))
                |> Maybe.withDefault 1
                |> String.fromFloat

        isSelected =
            selIdx /= Nothing

        selectionMsg =
            ToggleSelection idx isSelected

        animAttrs =
            if isSelected then
                [ style "transform" ([ "scale(", scaleForSelection, ")" ] |> String.join "")
                , style "transition" "transform 500ms"
                ]

            else
                [ style "transition" "transform 500ms" ]
    in
    Html.td
        [ PE.onPrimaryEnterAndDown selectionMsg
        , PE.onPrimaryDown selectionMsg
        ]
        [ div
            (animAttrs
                ++ [ class "br3 w3 h3 flex"
                   , class "relative"
                   , class
                        (case c of
                            G.Water ->
                                "bg-light-blue"

                            G.Wall ->
                                "bg-light-purple white"

                            G.Empty ->
                                ""

                            G.Seed ->
                                "bg-light-pink "
                        )
                   ]
            )
            [ div
                [ class "code f3"
                , class "absolute pa2"
                ]
                [ text (selIdx |> Maybe.map String.fromInt |> Maybe.withDefault "")
                ]
            ]
        ]


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
