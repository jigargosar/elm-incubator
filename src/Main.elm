module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (autofocus, class, style, value)
import Html.Events exposing (onFocus)
import Json.Decode as JD exposing (Decoder)



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type Model
    = Model SearchInput


type SearchInput
    = SI String Bool


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Model (SI "foo bar" False)
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | ShowResults Bool
    | SIUp
    | SIDown


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        ShowResults showResults ->
            ( case model of
                Model (SI v _) ->
                    Model (SI v showResults)
            , Cmd.none
            )

        SIUp ->
            ( model, Cmd.none )

        SIDown ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onClickOutside siDomId (ShowResults False)
        ]


onClickOutside : String -> msg -> Sub msg
onClickOutside domId msg =
    Browser.Events.onClick
        (pathDomIdsDecoder
            |> JD.andThen
                (\pathIds ->
                    if List.member domId pathIds then
                        JD.fail "inside"

                    else
                        JD.succeed msg
                )
        )


nonEmpty : String -> Maybe String
nonEmpty s =
    case s of
        "" ->
            Nothing

        _ ->
            Just s


pathDomIdsDecoder : Decoder (List String)
pathDomIdsDecoder =
    let
        nonEmptyDomIdDecoder : Decoder (Maybe String)
        nonEmptyDomIdDecoder =
            JD.maybe (JD.field "id" JD.string)
                |> JD.map (Maybe.andThen nonEmpty)
    in
    JD.field "path"
        (JD.list nonEmptyDomIdDecoder |> JD.map (List.filterMap identity))



-- View


view : Model -> Html Msg
view (Model si) =
    div [ class "pt4 measure-wide center" ]
        [ viewSearch si
        ]


viewSearch (SI v showResults) =
    if showResults then
        viewSearchWithResults v

    else
        viewSearchSimple v


siDomId =
    "si-dom-id"


viewSearchWithResults v =
    div
        [ Html.Attributes.id siDomId
        , class "pv2 ph3"
        , class "ba b--transparent shadow-1"
        , class "flex flex-column"
        , style "border-radius" "1.25rem"
        ]
        [ viewIP v
        , div [] (List.map viewRI [ "result 1", "result 1", "result 1", "result 1" ])
        ]


viewSearchSimple v =
    div
        [ Html.Attributes.id siDomId
        , class "pv2 ph3"
        , class "ba b--moon-gray "
        , class "fw-b--transparent fw-shadow-1"
        , class "flex flex-column"
        , class "br-pill"
        ]
        [ viewIP v
        ]


viewRI t =
    div [ class "f5 lh-title ttc" ] [ text t ]


viewIP v =
    input
        [ class "bg-transparent bn outline-0"
        , class "lh-title flex-auto"
        , value v
        , onFocus (ShowResults True)
        , autofocus True
        , Html.Events.preventDefaultOn "keydown"
            (keyDecoder
                |> JD.andThen
                    (\key ->
                        case key of
                            "Escape" ->
                                JD.succeed ( ShowResults False, False )

                            "Tab" ->
                                JD.succeed ( ShowResults False, False )

                            "ArrowUp" ->
                                JD.succeed ( SIUp, True )

                            "ArrowDown" ->
                                JD.succeed ( SIDown, True )

                            _ ->
                                JD.fail "nah!"
                    )
            )
        ]
        []


keyDecoder : Decoder String
keyDecoder =
    JD.field "key" JD.string
