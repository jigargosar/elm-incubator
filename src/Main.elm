port module Main exposing (main)

import Browser
import Browser.Events
import Css exposing (..)
import Html.Styled exposing (Html, div, input, styled, text)
import Html.Styled.Attributes as A exposing (attribute, autofocus, class, tabindex, value)
import Html.Styled.Events as E exposing (onFocus, onInput)
import Json.Decode as JD exposing (Decoder)


port onFocusOutside : (String -> msg) -> Sub msg



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.Styled.toUnstyled
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


queryInputValue : Query -> Html.Styled.Attribute msg
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
   when to show/hide search suggestions?

   MUST HideSuggestions:
   * when any element outside widget receives focus.
   * on Escape key, when search widget input has focus.
   * on Click, outside widget.

   MAYBE HideSuggestions:
   * on Escape key, when focus in within widget.
   * on Escape key, when focus is on any of search suggestion.

-}


type Msg
    = NoOp
    | QFocused
    | HideSuggestions
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

        HideSuggestions ->
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
        [ onClickOutside siContainerDomId HideSuggestions
        , onFocusOutside
            (\domId ->
                if domId == siContainerDomId then
                    HideSuggestions
                    --NoOp

                else
                    NoOp
            )
        ]



-- View


type alias HM =
    Html Msg


view : Model -> HM
view (Model si) =
    div [ class "pt4 measure-wide center" ]
        [ div [ A.id "above-si-dom-id", tabindex 0 ] [ text "above si" ]
        , viewSearchWidget si
        , div [ A.id "below-si-dom-id", tabindex 0 ] [ text "below si" ]
        ]


viewSearchWidget (SI qs showSuggestions) =
    let
        suggestions =
            [ "suggestion 1", "suggestion 1", "suggestion 1", "suggestion 1" ]

        suggestionView =
            div [] (List.map viewSuggestionItem suggestions)

        inputView =
            styled div
                (flex auto
                    :: padding2 sp2 sp3
                    :: displayFlex
                    :: widgetBorder
                    :: (if showSuggestions then
                            [ widgetShadow1
                            , bTransparent
                            , brTop
                            ]

                        else
                            [ focusWithin [ widgetShadow1, bTransparent ]
                            , hover [ widgetShadow1, bTransparent ]
                            ]
                       )
                )
                []
                [ viewSearchInput qs ]
    in
    styled div
        [ displayFlex, position relative ]
        [ A.id siContainerDomId
        , attribute "data-focus-outside" siContainerDomId
        ]
        [ inputView
        , if showSuggestions then
            --, div [ class "mh3 bb " ] []
            styled div
                [ -- layout
                  position absolute
                , top (pct 100)
                , width <| pct 100

                -- style
                , widgetBorder
                , bTransparent
                , brBottom
                , widgetShadow2
                , backgroundColor white
                ]
                []
                [ widgetSeparator
                , styled div [ padding2 sp2 zero ] [] [ suggestionView ]
                ]

          else
            text ""
        ]


widgetSeparator =
    styled div [ margin2 zero sp3, borderTop3 (px 1) solid wbColor ] [] []



--tapDecoder msg tap decoder =
--    JD.oneOf [ tap |> andThenLogFail2 msg, decoder ]


wbColor =
    -- rgba 223 225 229 0
    hex "#dfe1e5"


widgetBorder =
    batch
        [ border3 (px 1) solid wbColor
        , borderRadius (rem 1.25)
        ]


widgetShadow1 =
    boxShadowL [ "0 1px 6px 0 rgba(32, 33, 36, 0.28)" ]


widgetShadow2 =
    boxShadowL [ "0 4px 6px 0 rgba(32, 33, 36, 0.28)" ]



-- WIDGET ROOT COMMON ATTRS


siContainerDomId =
    "si-container-dom-id"


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


andThenLogFail2 msg =
    JD.andThen
        (\v ->
            let
                _ =
                    Debug.log msg v
            in
            JD.fail ""
        )



--noinspection ElmUnusedSymbol


logFail : a -> Decoder b
logFail v =
    let
        _ =
            Debug.log "v" v
    in
    JD.fail ""



-- WIDGET SUGGESTIONS VIEW


viewSuggestionItem t =
    div
        [ class "ph3 pv1 f5 lh-copy ttc"
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



--isNullDecoder =
--    JD.oneOf [ JD.null True, JD.succeed False ]


widgetInputKeyDownDecoder : String -> Decoder ( Msg, Bool )
widgetInputKeyDownDecoder key =
    case key of
        "Escape" ->
            JD.succeed ( HideSuggestions, False )

        --"Tab" ->
        --    JD.succeed ( HideSuggestions, False )
        "ArrowUp" ->
            JD.succeed ( QCursorUp, True )

        "ArrowDown" ->
            JD.succeed ( QCursorDown, True )

        _ ->
            JD.fail "nah!"


keyDecoder : Decoder String
keyDecoder =
    JD.field "key" JD.string



-- CSS HELPERS


sp2 =
    rem 0.5


sp3 =
    rem 1


white =
    hex "#fff"


brTop =
    batch
        [ borderBottomLeftRadius zero
        , borderBottomRightRadius zero
        ]


brBottom =
    batch
        [ borderTopLeftRadius zero
        , borderTopRightRadius zero
        ]


bTransparent =
    borderColor transparent


focusWithin =
    Css.pseudoClass "focus-within"


boxShadowL ls =
    Css.property "box-shadow" (String.join ";" ls)



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
