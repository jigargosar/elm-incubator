module GameRunner exposing (main)

import AbstractGame as G
import Basics.Extra exposing (swap)
import Browser exposing (Document)
import Dict
import Grid exposing (GI)
import Html exposing (Html, button, div, node, table, text)
import Html.Attributes exposing (autofocus, class, style)
import Html.Events as HE exposing (onClick, onMouseEnter)
import Json.Decode as D exposing (Decoder)
import List.Extra
import Set exposing (Set)



-- Model


type Model
    = Running (Set GI) G.GameModel
    | Over G.Info
    | Won G.Info


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( Running Set.empty G.initGame
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | PlayAnother
    | Collect
    | OnClick GI


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        PlayAnother ->
            case model of
                Running _ _ ->
                    ( model, Cmd.none )

                Over _ ->
                    init ()

                Won _ ->
                    init ()

        OnClick idx ->
            case model of
                Running selected g ->
                    let
                        nm =
                            Running
                                (if Set.member idx selected then
                                    Set.remove idx selected

                                 else
                                    Set.insert idx selected
                                )
                                g
                    in
                    ( nm, Cmd.none )

                Over _ ->
                    ( model, Cmd.none )

                Won _ ->
                    ( model, Cmd.none )

        Collect ->
            case model of
                Running selected g ->
                    let
                        nm =
                            case G.makeMove (Set.toList selected) g of
                                G.InvalidMove ->
                                    model

                                G.NextState ng ->
                                    Running Set.empty ng

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
                    Running sel g ->
                        [ div [ class "pa3" ] [ text "Game Running" ]
                        , viewGameInfo sel (G.info g)
                        , div [ class "pa3" ]
                            [ btn
                                Collect
                                "collect"
                            ]
                        ]

                    Over info ->
                        [ div [ class "pa3" ] [ text "Game Lost" ]
                        , viewGameInfo Set.empty info
                        , div [ class "pa3" ] [ btn PlayAnother "Play Again?" ]
                        ]

                    Won info ->
                        [ div [ class "pa3" ] [ text "Game Won" ]
                        , viewGameInfo Set.empty info
                        , div [ class "pa3" ] [ btn PlayAnother "Play Again?" ]
                        ]
               )
        )


type alias HM =
    Html Msg


viewGameInfo : Set GI -> G.Info -> HM
viewGameInfo sel i =
    div []
        [ div [ class "pa3" ]
            [ text (Debug.toString { currentTarget = i.currentTarget, movesLeft = i.movesLeft })
            ]
        , viewGameCells sel i.fallen (Grid.toList i.grid)
        ]


type alias PE =
    { isPrimary : Bool
    , pressure : Float
    , target : El
    , offsetX : Float
    , offsetY : Float
    , pageX : Float
    , pageY : Float
    }


type alias El =
    { offsetLeft : Float
    , offsetTop : Float
    , offsetWidth : Float
    , offsetHeight : Float
    }


elDecoder =
    D.map4 El
        (D.field "offsetLeft" D.float)
        (D.field "offsetTop" D.float)
        (D.field "offsetWidth" D.float)
        (D.field "offsetHeight" D.float)


andMap =
    D.map2 (|>)


peDecoder : Decoder PE
peDecoder =
    D.succeed PE
        |> andMap (D.field "isPrimary" D.bool)
        |> andMap (D.field "pressure" D.float)
        |> andMap (D.field "target" elDecoder)
        |> andMap (D.field "offsetX" D.float)
        |> andMap (D.field "offsetY" D.float)
        |> andMap (D.field "pageX" D.float)
        |> andMap (D.field "pageY" D.float)
        |> andThenTapLog "PE"


andThenTapLog logMsg =
    D.andThen (\v -> D.succeed (Debug.log logMsg v))



--noinspection ElmUnusedSymbol


andThenDebugFail =
    D.andThen (Debug.log "debug" >> (\_ -> D.fail ""))


viewGameCells : Set GI -> List ( GI, GI ) -> List ( GI, G.Cell ) -> HM
viewGameCells sel fallen cells =
    let
        fallenDict =
            Dict.fromList (List.map swap fallen)

        viewCell ( ( _, _ ) as idx, c ) =
            Html.td
                [ onClick (OnClick idx)
                , HE.on "pointerenter"
                    (peDecoder
                        |> D.andThen
                            (\pe ->
                                if
                                    pe.isPrimary
                                        && pe.pressure
                                        >= 0.5
                                        && (pe.offsetX > (pe.target.offsetWidth * 0.2))
                                        && (pe.offsetX < (pe.target.offsetWidth - pe.target.offsetWidth * 0.2))
                                then
                                    D.succeed (OnClick idx)

                                else
                                    D.fail ""
                            )
                    )
                ]
                [ div
                    [ class "br3 w3 h3 flex"
                    , class "relative"
                    , style "transform"
                        (if Set.member idx sel then
                            "scale(0.5)"

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
