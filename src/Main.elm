module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Css exposing (..)
import Html.Styled exposing (Html, div, input, styled, text)
import Html.Styled.Attributes as A exposing (class, tabindex, value)
import Html.Styled.Events as E exposing (onFocus, onInput)
import Json.Decode as JD exposing (Decoder)
import Task exposing (Task)



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
    = Model SearchWidget



-- SEARCH WIDGET MODEL


type SearchWidget
    = SW Query Suggestions


updateQueryOnInput : String -> SearchWidget -> SearchWidget
updateQueryOnInput typed (SW (Query o _) ss) =
    SW (Query o (Typed typed)) (showSuggestions ss |> Maybe.withDefault ss)


selectPrev : SearchWidget -> SearchWidget
selectPrev (SW q ss) =
    case ss of
        VisibleSelected ( lh :: lt, c, r ) ->
            SW (overrideQueryInput lh q) (VisibleSelected ( lt, lh, c :: r ))

        VisibleSelected ( [], c, r ) ->
            case nelReverse ( c, r ) of
                ( h, t ) ->
                    SW (overrideQueryInput h q) (VisibleSelected ( t, h, [] ))

        VisibleNoneSelected ( c, r ) ->
            selectPrev (SW q (VisibleSelected ( [], c, r )))

        Hidden nel ->
            SW q (VisibleNoneSelected nel)


selectNext : SearchWidget -> SearchWidget
selectNext (SW q ss) =
    case ss of
        VisibleSelected ( l, c, rh :: rt ) ->
            SW (overrideQueryInput rh q) (VisibleSelected ( c :: l, rh, rt ))

        VisibleSelected ( l, c, [] ) ->
            case nelReverse ( c, l ) of
                ( h, t ) ->
                    SW (overrideQueryInput h q) (VisibleSelected ( [], h, t ))

        VisibleNoneSelected ( c, r ) ->
            SW (overrideQueryInput c q) (VisibleSelected ( [], c, r ))

        Hidden nel ->
            SW q (VisibleNoneSelected nel)


showSuggestionsIfOriginalQuery : SearchWidget -> SearchWidget
showSuggestionsIfOriginalQuery ((SW q ss) as sw) =
    case ( isQueryOriginal q, showSuggestions ss ) of
        ( True, Just nss ) ->
            SW q nss

        _ ->
            sw


hideSuggestionsIfShown : SearchWidget -> SearchWidget
hideSuggestionsIfShown (SW q ss) =
    SW q
        (case ss of
            Hidden _ ->
                ss

            VisibleNoneSelected nel ->
                Hidden nel

            VisibleSelected lcr ->
                Hidden (lcrToNel lcr)
        )


hideSuggestionsAndRevertInputOverride : SearchWidget -> SearchWidget
hideSuggestionsAndRevertInputOverride (SW (Query o iv) ss) =
    let
        niv =
            case iv of
                Typed _ ->
                    iv

                Overridden ty _ ->
                    Typed ty

        nss =
            case ss of
                Hidden _ ->
                    ss

                VisibleNoneSelected nel ->
                    Hidden nel

                VisibleSelected lcr ->
                    Hidden (lcrToNel lcr)
    in
    SW (Query o niv) nss



-- QUERY


type Query
    = Query String InputValue


type InputValue
    = Typed String
    | Overridden String String


inputValueToString : InputValue -> String
inputValueToString iv =
    case iv of
        Typed string ->
            string

        Overridden _ string ->
            string


initQuery : String -> Query
initQuery string =
    Query string (Typed string)


setQueryInputTyped : String -> Query -> Query
setQueryInputTyped to (Query o _) =
    Query o (Typed to)


overrideQueryInput : String -> Query -> Query
overrideQueryInput to (Query o iv) =
    let
        newIV =
            case iv of
                Typed ty ->
                    Overridden ty to

                Overridden ty _ ->
                    Overridden ty to
    in
    Query o newIV


queryInputValue : Query -> Html.Styled.Attribute msg
queryInputValue (Query _ c) =
    value (inputValueToString c)


isQueryOriginal : Query -> Bool
isQueryOriginal (Query o c) =
    o == inputValueToString c



-- SUGGESTIONS


type Suggestions
    = Hidden (NEL String)
    | VisibleNoneSelected (NEL String)
    | VisibleSelected (LCR String)


initialSuggestionNEL : NEL String
initialSuggestionNEL =
    ( "Suggestion 0 ", [ "suggestion 1", "suggestion 1", "suggestion 1", "suggestion 1" ] )


showSuggestions : Suggestions -> Maybe Suggestions
showSuggestions ss =
    case ss of
        VisibleSelected _ ->
            Nothing

        VisibleNoneSelected _ ->
            Nothing

        Hidden nel ->
            Just (VisibleNoneSelected nel)



-- NON EMPTY LIST


type alias NEL a =
    ( a, List a )


nelToList : NEL a -> List a
nelToList ( h, t ) =
    h :: t


nelReverse : ( a, List a ) -> ( a, List a )
nelReverse (( h, t ) as nel) =
    case List.reverse (h :: t) of
        [] ->
            -- Will never happen
            nel

        lst :: rest ->
            ( lst, rest )



-- LIST ZIPPER


type alias LCR a =
    ( List a, a, List a )


lcrToNel : LCR a -> NEL a
lcrToNel ( l, c, r ) =
    case List.reverse l of
        head :: tail ->
            ( head, tail ++ c :: r )

        [] ->
            ( c, r )


lcrToList : LCR a -> List a
lcrToList ( l, c, r ) =
    List.reverse l ++ c :: r


lcrMapCS : (a -> b) -> (a -> b) -> LCR a -> LCR b
lcrMapCS fc fs ( l, c, r ) =
    ( List.map fs l, fc c, List.map fs r )



-- INIT


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Model (SW (initQuery "foo bar") (Hidden initialSuggestionNEL))
    , focusSI
    )


focusSI =
    Dom.focus siDomId |> Task.attempt OnFocusResult



-- Update
{-
   when to show/hide search suggestions?

   MUST HideSuggestions:
   * when any element outside widget receives focus.
   * on Escape key, when search widget input has focus.
   * on Click outside widget.

   MAYBE HideSuggestions:
   * on Escape key, when focus in within widget.
   * on Escape key, when focus is on any of search suggestion.

-}


type Msg
    = NoOp
    | OnFocusResult (Result Dom.Error ())
    | QInputFocused
    | HideSuggestions
    | QInputChanged String
    | QInputSelectPrev
    | QInputSelectNext
    | OnQInputEsc


update : Msg -> Model -> ( Model, Cmd Msg )
update message ((Model sw) as model) =
    case message of
        NoOp ->
            ( model, Cmd.none )

        OnFocusResult (Err (Dom.NotFound domId)) ->
            let
                _ =
                    Debug.log "FocusError" domId
            in
            ( model, Cmd.none )

        OnFocusResult (Ok ()) ->
            ( model, Cmd.none )

        QInputFocused ->
            ( Model (showSuggestionsIfOriginalQuery sw)
            , Cmd.none
            )

        HideSuggestions ->
            ( Model (hideSuggestionsIfShown sw)
            , Cmd.none
            )

        QInputChanged changed ->
            ( Model (updateQueryOnInput changed sw)
            , Cmd.none
            )

        QInputSelectPrev ->
            ( Model (selectPrev sw), Cmd.none )

        QInputSelectNext ->
            ( Model (selectNext sw), Cmd.none )

        OnQInputEsc ->
            ( Model (hideSuggestionsAndRevertInputOverride sw), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onClickOutside searchWidgetDomId HideSuggestions
        ]



-- View


type alias HM =
    Html Msg


searchWidgetDomId =
    "search-widget-dom-id"


view : Model -> HM
view (Model si) =
    div
        [ class "pt4 measure-wide center"
        , E.on "focusin"
            (succeedWhen (activeElOutside searchWidgetDomId) HideSuggestions)
        ]
        [ div [ A.id "above-si-dom-id", tabindex 0 ] [ text "above si" ]
        , viewSearchWidget si
        , div [ A.id "below-si-dom-id", tabindex 0 ] [ text "below si" ]
        ]


viewSearchWidget : SearchWidget -> HM
viewSearchWidget (SW qs ss) =
    let
        maybeSuggestionsView =
            viewSuggestions ss

        areSuggestionsVisible =
            maybeSuggestionsView /= Nothing

        inputView =
            styled div
                (flex auto
                    :: padding2 sp2 sp3
                    :: displayFlex
                    :: borderWidget
                    :: (if areSuggestionsVisible then
                            [ shadowWidgetInput, borderTransparent, borderRadiusOnlyTop ]

                        else
                            [ hoverAndFocusWithin [ shadowWidgetInput, borderTransparent ] ]
                       )
                )
                []
                [ viewSearchInput qs ]
    in
    styled div
        [ displayFlex, position relative ]
        [ A.id searchWidgetDomId
        ]
        [ inputView
        , maybeSuggestionsView
            |> viewMaybe
                (\suggestionsView ->
                    styled div
                        [ -- layout
                          absolute
                        , top p100
                        , w100

                        -- style
                        , borderRadiusWidget
                        , borderRadiusOnlyBottom
                        , overflow hidden -- to ensure children don't overflow border radius
                        , shadowWidgetSuggestions
                        , backgroundColor white
                        ]
                        []
                        [ widgetSeparator
                        , styled div [ padding2 sp2 zero ] [] [ suggestionsView ]
                        ]
                )
        ]


hoverAndFocusWithin : List Style -> Style
hoverAndFocusWithin styles =
    batch [ hover styles, focusWithin styles ]


viewMaybe : (a -> Html msg) -> Maybe a -> Html msg
viewMaybe func mb =
    case mb of
        Just v ->
            func v

        Nothing ->
            text ""


viewSuggestions : Suggestions -> Maybe HM
viewSuggestions ss =
    case ss of
        VisibleSelected lcr ->
            div
                []
                (lcrMapCS viewSelectedSuggestionItem viewSuggestionItem lcr
                    |> lcrToList
                )
                |> Just

        VisibleNoneSelected nel ->
            div [] (List.map viewSuggestionItem (nelToList nel))
                |> Just

        Hidden _ ->
            Nothing


widgetSeparator : Html msg
widgetSeparator =
    styled div [ margin2 zero sp3, borderTop3 (px 1) solid wbColor ] [] []



--tapDecoder msg tap decoder =
--    JD.oneOf [ tap |> andThenLogFail2 msg, decoder ]


wbColor =
    -- rgba 223 225 229 0
    hex "#dfe1e5"


borderWidget =
    batch
        [ border3 (px 1) solid wbColor
        , borderRadiusWidget
        ]


borderRadiusWidget =
    borderRadius (rem 1.25)


shadowWidgetInput =
    boxShadowL [ "0 1px 6px 0 rgba(32, 33, 36, 0.28)" ]


shadowWidgetSuggestions =
    boxShadowL [ "0 4px 6px 0 rgba(32, 33, 36, 0.28)" ]



-- ELEMENT DECODERS


succeedWhen predDecoder tag =
    predDecoder |> JD.andThen (succeedWhenTrue tag)


activeElOutside domId =
    activeElementDecoder |> JD.map (isElOutside domId)


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


activeElementDecoder : Decoder El
activeElementDecoder =
    JD.at [ "target", "ownerDocument", "activeElement" ] elDecoder



--noinspection ElmUnusedSymbol


andThenLogFail2 : String -> Decoder a -> Decoder b
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


viewSelectedSuggestionItem : String -> HM
viewSelectedSuggestionItem t =
    div
        [ class "ph3 pv1 f5 lh-copy ttc"
        , class "bg-light-gray"
        , tabindex -1
        ]
        [ text t ]


viewSuggestionItem : String -> HM
viewSuggestionItem t =
    div
        [ class "ph3 pv1 f5 lh-copy ttc"
        , tabindex -1
        ]
        [ text t ]



-- WIDGET INPUT VIEW


siDomId =
    "si-dom-id"


viewSearchInput : Query -> HM
viewSearchInput qs =
    input
        [ A.id siDomId
        , class "bg-transparent bn outline-0"
        , class "lh-title flex-auto"
        , onFocus QInputFocused
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
            JD.succeed ( OnQInputEsc, False )

        --"Tab" ->
        --    JD.succeed ( HideSuggestions, False )
        "ArrowUp" ->
            JD.succeed ( QInputSelectPrev, True )

        "ArrowDown" ->
            JD.succeed ( QInputSelectNext, True )

        _ ->
            JD.fail "nah!"


keyDecoder : Decoder String
keyDecoder =
    JD.field "key" JD.string



-- CSS HELPERS


absolute =
    position Css.absolute


p100 =
    pct 100


w100 =
    width (pct 100)


sp2 =
    rem 0.5


sp3 =
    rem 1


white =
    hex "#fff"


borderRadiusOnlyTop =
    batch
        [ borderBottomLeftRadius zero
        , borderBottomRightRadius zero
        ]


borderRadiusOnlyBottom =
    batch
        [ borderTopLeftRadius zero
        , borderTopRightRadius zero
        ]


borderTransparent =
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
