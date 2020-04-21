module GameRunner exposing (main)

import AbstractGame as G
import Browser exposing (Document)
import Grid exposing (GI)
import Html exposing (Html, button, div, node, table, text)
import Html.Attributes exposing (autofocus, class, style)
import Html.Events exposing (onClick)
import List.Extra
import Maybe.Extra
import PointerEvents as PE


updateSelection idx wasSelected moveBuilder =
    let
        moves =
            G.toStack moveBuilder
    in
    if wasSelected && List.member idx moves then
        -- Remove
        Maybe.Extra.oneOf [ G.clearIfOnlyEq idx, G.popIfSecondLastEq idx ] moveBuilder

    else if not wasSelected && not (List.member idx moves) then
        -- Add
        G.pushIdx idx moveBuilder

    else
        -- NoOp
        Nothing



-- Model


type Model
    = Running G.MoveBuilder
    | Over G.Info
    | Won G.Info


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( Running (G.initGame |> G.initMoveBuilder)
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

        ToggleSelection idx bool ->
            case model of
                Running moveBuilder ->
                    let
                        nm =
                            Running (updateSelection idx bool moveBuilder |> Maybe.withDefault moveBuilder)
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
                        , viewGameInfo (G.toStack moveBuilder) (G.info moveBuilder)
                        , div [ class "pa3" ]
                            [ btn
                                Collect
                                "collect"
                            ]
                        ]

                    Over info ->
                        [ div [ class "pa3" ] [ text "Game Lost" ]
                        , viewGameInfo [] info
                        , div [ class "pa3" ] [ btn PlayAnother "Play Again?" ]
                        ]

                    Won info ->
                        [ div [ class "pa3" ] [ text "Game Won" ]
                        , viewGameInfo [] info
                        , div [ class "pa3" ] [ btn PlayAnother "Play Again?" ]
                        ]
               )
        )


type alias HM =
    Html Msg


type alias Selection =
    List GI


viewGameInfo : Selection -> G.Info -> HM
viewGameInfo sel i =
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
        , viewGameCells sel (Grid.toList i.grid)
        ]


viewGameCells : Selection -> List ( GI, G.Cell ) -> HM
viewGameCells sel cells =
    let
        rows =
            List.Extra.gatherEqualsBy (Tuple.first >> Tuple.second) cells

        viewRow : Int -> ( ( GI, G.Cell ), List ( GI, G.Cell ) ) -> HM
        viewRow y ( h, t ) =
            Html.tr []
                (styledTH [ text "y", text (String.fromInt y) ]
                    :: List.map (viewCell sel) (h :: t)
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


viewCell : Selection -> ( GI, G.Cell ) -> HM
viewCell sel ( ( _, _ ) as idx, c ) =
    let
        selIdx =
            List.reverse sel |> List.Extra.elemIndex idx

        isSelected =
            selIdx /= Nothing

        selectionMsg =
            ToggleSelection idx isSelected
    in
    Html.td
        [ PE.onPrimaryEnterAndDown selectionMsg
        , PE.onPrimaryDown selectionMsg
        ]
        [ div
            [ class "br3 w3 h3 flex"
            , class "relative"
            , style "transform"
                (if isSelected then
                    "scale(0.75)"

                 else
                    "scale(1)"
                )
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
            [ div
                [ class "code f3 _o-50 glow"
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
