module Main exposing (main)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (autofocus, class, style, value)
import Html.Events exposing (onBlur, onFocus)



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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



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


viewSearchWithResults v =
    div
        [ class "pv2 ph3"
        , class "ba b--moon-gray "
        , class "fw-b--transparent fw-shadow-1"
        , class "flex flex-column"
        , style "border-radius" "1.25rem"
        ]
        [ viewIP v
        , div [] (List.map viewRI [ "result 1", "result 1", "result 1", "result 1" ])
        ]


viewSearchSimple v =
    div
        [ class "pv2 ph3"
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
        , onBlur (ShowResults False)
        , autofocus True
        ]
        []
