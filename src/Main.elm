module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (autofocus, class, style, value)
import Html.Events exposing (onFocus, onInput)
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
    | ShowResults
    | HideResults
    | QChanged String
    | QCursorUp
    | QCursorDown


update : Msg -> Model -> ( Model, Cmd Msg )
update message ((Model (SI qs sr)) as model) =
    case message of
        NoOp ->
            ( model, Cmd.none )

        ShowResults ->
            ( Model (SI qs True), Cmd.none )

        HideResults ->
            ( Model (SI qs False), Cmd.none )

        QChanged nqs ->
            ( Model (SI nqs sr)
            , Cmd.none
            )

        QCursorUp ->
            ( model, Cmd.none )

        QCursorDown ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onClickOutside siDomId HideResults
        ]



-- View


view : Model -> Html Msg
view (Model si) =
    div [ class "pt4 measure-wide center" ]
        [ viewSearch si
        ]


viewSearch (SI qs showResults) =
    if showResults then
        viewSearchWithResults qs

    else
        viewSearchSimple qs


siDomId =
    "si-dom-id"


viewSearchWithResults qs =
    div
        [ Html.Attributes.id siDomId
        , class "pv2 ph3"
        , class "ba b--transparent shadow-1"
        , style "border-radius" "1.25rem"
        , class "flex flex-column"
        ]
        [ viewSearchInput qs
        , div [] (List.map viewRI [ "result 1", "result 1", "result 1", "result 1" ])
        ]


viewSearchSimple qs =
    div
        [ Html.Attributes.id siDomId
        , class "pv2 ph3"
        , class "ba b--moon-gray "
        , class "fw-b--transparent fw-shadow-1"
        , class "br-pill"
        , class "flex flex-column"
        ]
        [ viewSearchInput qs
        ]


viewRI t =
    div [ class "f5 lh-title ttc" ] [ text t ]


viewSearchInput qs =
    input
        [ class "bg-transparent bn outline-0"
        , class "lh-title flex-auto"
        , autofocus True
        , onFocus ShowResults
        , onInput QChanged
        , value qs
        , Html.Events.preventDefaultOn "keydown"
            (JD.andThen keyDownDispatcher keyDecoder)
        ]
        []


keyDownDispatcher : String -> Decoder ( Msg, Bool )
keyDownDispatcher key =
    case key of
        "Escape" ->
            JD.succeed ( HideResults, False )

        "Tab" ->
            JD.succeed ( HideResults, False )

        "ArrowUp" ->
            JD.succeed ( QCursorUp, True )

        "ArrowDown" ->
            JD.succeed ( QCursorDown, True )

        _ ->
            JD.fail "nah!"


keyDecoder : Decoder String
keyDecoder =
    JD.field "key" JD.string



-- ON CLICK OUTSIDE SUB


onClickOutside : String -> msg -> Sub msg
onClickOutside domId msg =
    let
        func pathIds =
            if List.member domId pathIds then
                JD.fail "inside"

            else
                JD.succeed msg
    in
    Browser.Events.onClick
        (JD.andThen func pathDomIdsDecoder)


pathDomIdsDecoder : Decoder (List String)
pathDomIdsDecoder =
    JD.field "path"
        (JD.list maybeDomIdDecoder
            |> JD.map (List.filterMap identity)
        )


maybeDomIdDecoder : Decoder (Maybe String)
maybeDomIdDecoder =
    JD.oneOf
        [ JD.field "id" (JD.map nonEmpty JD.string)
        , JD.succeed Nothing
        ]


nonEmpty : String -> Maybe String
nonEmpty s =
    case s of
        "" ->
            Nothing

        _ ->
            Just s
