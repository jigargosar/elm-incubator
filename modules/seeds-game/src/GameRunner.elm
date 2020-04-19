module GameRunner exposing (main)

import AbstractGame as G
import Browser exposing (Document)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (autofocus, class)
import Html.Events exposing (onClick)



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
    | MakeMove Int
    | PlayAnother


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        MakeMove n ->
            case model of
                Running g ->
                    let
                        nm =
                            case G.makeMove n g of
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

        PlayAnother ->
            case model of
                Running _ ->
                    ( model, Cmd.none )

                Over _ ->
                    init ()

                Won _ ->
                    init ()


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
                    [ button
                        [ onClick <| MakeMove 10, class "ma2", autofocus True ]
                        [ text "collect 10" ]
                    , button
                        [ onClick <| MakeMove 50, class "ma2", autofocus True ]
                        [ text "collect 50" ]
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


viewGameInfo info =
    div [ class "pa3" ] [ text (Debug.toString info) ]


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
