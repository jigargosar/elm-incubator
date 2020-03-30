module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div, input, text)
import Html.Attributes as A exposing (autofocus, class, style, tabindex, value)
import Html.Events as E exposing (onFocus, onInput)
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


type Query
    = Query String String


initQuery : String -> Query
initQuery string =
    Query string string


queryInputChange : String -> Query -> Query
queryInputChange to (Query o _) =
    Query o to


queryInputValue : Query -> Html.Attribute msg
queryInputValue (Query _ c) =
    value c


isQueryOriginal (Query o c) =
    o == c


type SearchInput
    = SI Query Bool


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Model (SI (initQuery "foo bar") False)
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | QFocused
    | HideResults
    | QInputChanged String
    | QCursorUp
    | QCursorDown


update : Msg -> Model -> ( Model, Cmd Msg )
update message ((Model (SI q sr)) as model) =
    case message of
        NoOp ->
            ( model, Cmd.none )

        QFocused ->
            ( Model (SI q (isQueryOriginal q || sr)), Cmd.none )

        HideResults ->
            ( Model (SI q False), Cmd.none )

        QInputChanged changed ->
            ( Model (SI (queryInputChange changed q) True)
            , Cmd.none
            )

        QCursorUp ->
            ( Model (SI q True), Cmd.none )

        QCursorDown ->
            ( Model (SI q True), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onClickOutside siContainerDomId HideResults
        ]



-- View


view : Model -> Html Msg
view (Model si) =
    div [ class "pt4 measure-wide center" ]
        [ viewSearch si
        , div [ A.id "rel-tar", tabindex 0 ] [ text "rel target div tabindex 0" ]
        ]


viewSearch (SI qs showResults) =
    if showResults then
        viewSearchWithResults qs

    else
        viewSearchSimple qs


siContainerDomId =
    "si-container-dom-id"


type alias HM =
    Html Msg


viewSearchWithResults : Query -> HM
viewSearchWithResults qs =
    div
        [ A.id siContainerDomId
        , class "pv2 ph3"
        , class "ba b--transparent shadow-1"
        , style "border-radius" "1.25rem"
        , class "flex flex-column"
        ]
        [ viewSearchInput qs
        , div [] (List.map viewRI [ "result 1", "result 1", "result 1", "result 1" ])
        ]


viewSearchSimple : Query -> HM
viewSearchSimple qs =
    div
        [ A.id siContainerDomId
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


viewSearchInput : Query -> HM
viewSearchInput qs =
    input
        [ A.id "si-dom-id"
        , class "bg-transparent bn outline-0"
        , class "lh-title flex-auto"
        , autofocus True
        , onFocus QFocused
        , onInput QInputChanged
        , onBlurActiveElementBody HideResults
        , queryInputValue qs
        , E.preventDefaultOn "keydown"
            (JD.andThen keyDownDispatcher keyDecoder)
        ]
        []


onBlurActiveElementBody : msg -> Html.Attribute msg
onBlurActiveElementBody msg =
    E.on "blur"
        (JD.at [ "target", "ownerDocument", "activeElement", "tagName" ] JD.string
            |> JD.andThen
                (\aeTagName ->
                    if aeTagName == "BODY" then
                        JD.succeed msg

                    else
                        JD.fail "on blur not body, i.e. ae didn't change"
                )
        )


keyDownDispatcher : String -> Decoder ( Msg, Bool )
keyDownDispatcher key =
    case key of
        "Escape" ->
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
