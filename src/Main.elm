module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div, input, text)
import Html.Attributes as A exposing (autofocus, class, style, tabindex, value)
import Html.Events as E exposing (onFocus, onInput)
import Json.Decode as JD exposing (Decoder)
import Json.Encode exposing (Value)



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
{-
   when to show/hide search results?

   MUST HideResults:
   * when any element outside widget receives focus.
   * on Escape key, when search widget input has focus.
   * on Click, outside widget.

   MAYBE HideResults:
   * on Escape key, when focus in within widget.
   * on Escape key, when focus is on any of search result.

-}


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


type alias HM =
    Html Msg


view : Model -> HM
view (Model si) =
    div [ class "pt4 measure-wide center" ]
        [ div [ A.id "above-si-dom-id", tabindex 0 ] [ text "above si" ]
        , viewSearch si
        , div [ A.id "below-si-dom-id", tabindex 0 ] [ text "below si" ]
        ]


viewSearch (SI qs showResults) =
    if showResults then
        viewSearchWithResults qs

    else
        viewSearchSimple qs


siContainerDomId =
    "si-container-dom-id"


viewSearchWithResults : Query -> HM
viewSearchWithResults qs =
    let
        attrs =
            [ class "pt2 ph3"
            , class "ba b--transparent shadow-1"
            , style "border-radius" "1.25rem"
            , class "flex flex-column"
            ]
    in
    div
        (attrs ++ commonWidgetAttrs)
        [ viewSearchInput qs
        , div [ class "pb2 ph2" ] (List.map viewResultItem [ "result 1", "result 1", "result 1", "result 1" ])
        ]


viewSearchSimple : Query -> HM
viewSearchSimple qs =
    let
        attrs =
            [ class "pv2 ph3"
            , class "ba b--moon-gray "
            , class "fw-b--transparent fw-shadow-1"
            , class "br-pill"
            , class "flex flex-column"
            ]
    in
    div
        (attrs ++ commonWidgetAttrs)
        [ viewSearchInput qs
        ]



-- WIDGET ROOT COMMON ATTRS


commonWidgetAttrs =
    [ A.id siContainerDomId
    , E.on "focusout"
        (JD.at [ "relatedTarget" ] elDecoder
            |> JD.andThen (isElOutside siContainerDomId >> succeedWhenTrue HideResults)
        )
    ]


succeedWhenTrue msg bool =
    if bool then
        JD.succeed msg

    else
        JD.fail "not true"


isElOutside : String -> El -> Bool
isElOutside domId (El id mel) =
    if id == domId then
        False

    else
        case mel of
            Nothing ->
                True

            Just el ->
                isElOutside domId el


type El
    = El String (Maybe El)


elDecoder : Decoder El
elDecoder =
    JD.map2 El
        (JD.field "id" JD.string)
        (JD.field "parentElement" (JD.nullable (JD.lazy (\_ -> elDecoder))))



--noinspection ElmUnusedSymbol


logFail : a -> Decoder b
logFail v =
    let
        _ =
            Debug.log "v" v
    in
    JD.fail ""



-- WIDGET RESULTS VIEW


viewResultItem t =
    div
        [ class "f5 lh-title ttc"
        , tabindex 0
        ]
        [ text t ]



-- WIDGET INPUT VIEW


viewSearchInput : Query -> HM
viewSearchInput qs =
    let
        domId =
            "si-dom-id"
    in
    input
        [ A.id domId
        , class "bg-transparent bn outline-0"
        , class "lh-title flex-auto"
        , autofocus True
        , onFocus QFocused
        , onInput QInputChanged
        , queryInputValue qs
        , E.preventDefaultOn "keydown"
            (JD.andThen widgetInputKeyDownDecoder keyDecoder)
        ]
        []


widgetInputKeyDownDecoder : String -> Decoder ( Msg, Bool )
widgetInputKeyDownDecoder key =
    case key of
        "Escape" ->
            JD.succeed ( HideResults, False )

        --"Tab" ->
        --    JD.succeed ( HideResults, False )
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
