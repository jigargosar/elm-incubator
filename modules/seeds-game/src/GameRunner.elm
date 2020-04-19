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
    | MakeMove


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        MakeMove ->
            case model of
                Running g ->
                    let
                        nm =
                            case G.makeMove 10 g of
                                G.InvalidMove ->
                                    model

                                G.GameOver info ->
                                    Over info

                                G.NextState ng ->
                                    Running ng
                    in
                    ( nm, Cmd.none )

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
        (case model of
            Running g ->
                [ div [ class "pa3" ] [ text "Game Running" ]
                , div [ class "pa3" ]
                    [ text (Debug.toString (G.info g))
                    ]
                , div
                    [ class "pa3"
                    , onClick MakeMove
                    , autofocus True
                    ]
                    [ button [ class "ma2" ] [ text "make move" ] ]
                ]

            Over info ->
                [ div [ class "pa3" ] [ text "Game Over" ]
                , div [ class "pa3" ]
                    [ text (Debug.toString info)
                    ]
                ]
        )



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
