module GameRunner exposing (main)

import AbstractGame as G
import Basics.Extra exposing (swap)
import Browser exposing (Document)
import Dict
import Grid exposing (GI)
import Html exposing (Html, button, div, table, text)
import Html.Attributes exposing (autofocus, class)
import Html.Events exposing (onClick)
import List.Extra



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
    | MakeMove (List GI)


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

        MakeMove list ->
            case model of
                Running g ->
                    let
                        nm =
                            case G.makeMove list g of
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
        (case model of
            Running g ->
                [ div [ class "pa3" ] [ text "Game Running" ]
                , viewGameInfo (G.info g)
                , div [ class "pa3" ]
                    [ btn
                        (MakeMove
                            [ ( 3, 1 )
                            , ( 2, 2 )
                            , ( 4, 2 )
                            , ( 3, 3 )
                            , ( 3, 1 )
                            ]
                        )
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


type alias HM =
    Html Msg


viewGameInfo : G.Info -> HM
viewGameInfo i =
    div []
        [ div [ class "pa3" ]
            [ text (Debug.toString { currentTarget = i.currentTarget, movesLeft = i.movesLeft })
            ]
        , viewGameCells i.fallen (Grid.toList i.grid)
        ]


viewGameCells : List ( GI, GI ) -> List ( GI, G.Cell ) -> Html msg
viewGameCells fallen cells =
    let
        fallenDict =
            Dict.fromList (List.map swap fallen)

        viewCell ( ( x, y ) as idx, c ) =
            Html.td []
                [ div
                    [ class "br3 w3 h3 flex"
                    , class "relative"
                    , class
                        (case c of
                            G.Water ->
                                "bg-light-blue"

                            G.Wall ->
                                "bg-light-purple white"

                            G.Empty ->
                                ""
                        )
                    ]
                    [ div
                        [ class "code f4 o-50 glow"
                        , class "absolute pa2"
                        ]
                        [ case Dict.get idx fallenDict of
                            Just ( fx, fy ) ->
                                div [] [ text (String.fromInt fx ++ "," ++ String.fromInt fy) ]

                            Nothing ->
                                --div [] [ text (String.fromInt x ++ "," ++ String.fromInt y) ]
                                text ""
                        ]
                    ]
                ]

        rows =
            List.Extra.gatherEqualsBy (Tuple.first >> Tuple.second) cells

        viewRow y ( h, t ) =
            Html.tr []
                (styledTH [ text "y", text (String.fromInt y) ]
                    :: List.map viewCell (h :: t)
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
