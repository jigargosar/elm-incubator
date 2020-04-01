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
    = SW String InputValue Suggestions


initSW : String -> NEL String -> SearchWidget
initSW qs suggestionsNel =
    SW qs (Typed qs) (NotVisible suggestionsNel)


updateQueryOnInput : String -> SearchWidget -> SearchWidget
updateQueryOnInput typed (SW o _ ss) =
    SW o (Typed typed) (ensureVisible ss)


selectPreviousSuggestion : SearchWidget -> SearchWidget
selectPreviousSuggestion (SW o iv ss) =
    case ss of
        NotVisible nel ->
            SW o iv (Visible (selectionFromNel nel))

        Visible nes ->
            let
                ( suggestion, newNES ) =
                    selectBackward nes
            in
            SW o (overrideInputValue suggestion iv) (Visible newNES)


selectNextSuggestion : SearchWidget -> SearchWidget
selectNextSuggestion (SW o iv ss) =
    case ss of
        NotVisible nel ->
            SW o iv (Visible (selectionFromNel nel))

        Visible nes ->
            let
                ( suggestion, newNES ) =
                    selectForward nes
            in
            SW o (overrideInputValue suggestion iv) (Visible newNES)


showSuggestionsIfOriginalQuery : SearchWidget -> SearchWidget
showSuggestionsIfOriginalQuery ((SW o iv ss) as sw) =
    let
        isQueryOriginal =
            o == inputValueToString iv
    in
    if isQueryOriginal then
        SW o iv (ensureVisible ss)

    else
        sw


hideSuggestionsIfShown : SearchWidget -> SearchWidget
hideSuggestionsIfShown (SW o iv ss) =
    SW o iv (ensureHidden ss)


hideSuggestionsAndRevertInputOverride : SearchWidget -> SearchWidget
hideSuggestionsAndRevertInputOverride (SW o iv ss) =
    let
        niv =
            case iv of
                Typed _ ->
                    iv

                Overridden ty _ ->
                    Typed ty
    in
    SW o niv (ensureHidden ss)


searchInputString : SearchWidget -> String
searchInputString (SW _ iv _) =
    inputValueToString iv



-- InputValue


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


overrideInputValue : String -> InputValue -> InputValue
overrideInputValue to iv =
    case iv of
        Typed ty ->
            Overridden ty to

        Overridden ty _ ->
            Overridden ty to



-- SUGGESTIONS


type Suggestions
    = NotVisible (NEL String)
    | Visible (NeSelection String)


ensureVisible : Suggestions -> Suggestions
ensureVisible ss =
    case ss of
        NotVisible nel ->
            Visible (selectionFromNel nel)

        Visible _ ->
            ss


ensureHidden : Suggestions -> Suggestions
ensureHidden ss =
    case ss of
        NotVisible _ ->
            ss

        Visible neSelection ->
            NotVisible (selectionToNel neSelection)



-- NON EMPTY LIST SELECTION


type NeSelection a
    = NoneSelected (NEL a)
    | Selected (LCR a)


selectionFromNel : NEL a -> NeSelection a
selectionFromNel nel =
    NoneSelected nel


selectionToList : NeSelection a -> List a
selectionToList neSelection =
    case neSelection of
        NoneSelected ( h, t ) ->
            h :: t

        Selected lcr ->
            lcrToList lcr


selectionToNel : NeSelection a -> NEL a
selectionToNel neSelection =
    case neSelection of
        NoneSelected nel ->
            nel

        Selected lcr ->
            lcrToNel lcr


selectBackward : NeSelection a -> ( a, NeSelection a )
selectBackward neSelection =
    case neSelection of
        NoneSelected ( h, t ) ->
            selectBackward <| Selected ( [], h, t )

        Selected ( lh :: lt, c, r ) ->
            ( lh, Selected ( lt, lh, c :: r ) )

        Selected ( [], c, r ) ->
            case nelReverse ( c, r ) of
                ( h, t ) ->
                    ( h, Selected ( t, h, [] ) )


selectForward : NeSelection a -> ( a, NeSelection a )
selectForward neSelection =
    case neSelection of
        NoneSelected ( h, t ) ->
            ( h, Selected ( [], h, t ) )

        Selected ( l, c, rh :: rt ) ->
            ( rh, Selected ( c :: l, rh, rt ) )

        Selected ( l, c, [] ) ->
            case nelReverse ( c, l ) of
                ( h, t ) ->
                    ( h, Selected ( [], h, t ) )


selectionMapSelectedAndRest : (a -> b) -> (a -> b) -> NeSelection a -> NeSelection b
selectionMapSelectedAndRest funcSelected funcOthers neSelection =
    case neSelection of
        NoneSelected ( h, t ) ->
            NoneSelected ( funcOthers h, List.map funcOthers t )

        Selected ( l, c, r ) ->
            Selected ( List.map funcOthers l, funcSelected c, List.map funcOthers r )



-- NON EMPTY LIST


type alias NEL a =
    ( a, List a )


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



-- INIT


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        initialSuggestionNEL : NEL String
        initialSuggestionNEL =
            ( "Suggestion 0 ", [ "suggestion 1", "suggestion 1", "suggestion 1", "suggestion 1" ] )
    in
    ( Model (initSW "foo bar" initialSuggestionNEL)
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
            ( Model (selectPreviousSuggestion sw), Cmd.none )

        QInputSelectNext ->
            ( Model (selectNextSuggestion sw), Cmd.none )

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
viewSearchWidget ((SW _ _ ss) as sw) =
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
                [ viewSearchInput (searchInputString sw) ]
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
        NotVisible _ ->
            Nothing

        Visible neSelection ->
            selectionMapSelectedAndRest viewSelectedSuggestionItem viewSuggestionItem neSelection
                |> selectionToList
                |> div []
                |> Just


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


viewSearchInput : String -> HM
viewSearchInput valueString =
    input
        [ A.id siDomId
        , class "bg-transparent bn outline-0"
        , class "lh-title flex-auto"
        , onFocus QInputFocused
        , onInput QInputChanged
        , value valueString
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
