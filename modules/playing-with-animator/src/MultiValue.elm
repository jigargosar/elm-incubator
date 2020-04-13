module MultiValue exposing (main)

import Animator
import Animator.Inline
import Browser exposing (Document)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra
import Time



-- Model


type alias Model =
    { checked : Animator.Timeline (List Bool) }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { checked = Animator.init [ False, False ]
      }
    , Cmd.none
    )



-- Update


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watching
            -- we tell the animator how
            -- to get the checked timeline using .checked
            .checked
            -- and we tell the animator how
            -- to update that timeline as well
            (\newChecked model ->
                { model | checked = newChecked }
            )


type alias Idx =
    Int


type Msg
    = NoOp
    | Tick Time.Posix
    | CheckIdx Idx Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Tick newTime ->
            ( model
                |> Animator.update newTime animator
              -- (5) - Updating our model using our animator and the current time.
            , Cmd.none
            )

        CheckIdx idx bool ->
            ( { model
                | checked =
                    -- (6) - Here we're adding a new state to our timeline.
                    model.checked
                        |> Animator.go Animator.verySlowly (List.Extra.setAt idx bool (Animator.current model.checked))
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ animator
            |> Animator.toSubscription Tick model
        ]



-- View


type alias DM =
    Document Msg


view : Model -> DM
view model =
    Document "MultiValue"
        [ text "Hello MultiValueCheckBoxes"
        , div [] (List.indexedMap (viewCheckbox model.checked) (Animator.current model.checked))
        ]


viewCheckbox checkedA idx isChecked =
    div
        [ Animator.Inline.opacity checkedA <|
            \list ->
                if List.Extra.getAt idx list |> Maybe.withDefault isChecked then
                    Animator.at 1

                else
                    Animator.at 0.1
        , onClick (CheckIdx idx (not isChecked))
        , class "f4 pa2"
        ]
        [ text
            (String.fromInt idx
                ++ ": checked ? == "
                ++ (if isChecked then
                        "True"

                    else
                        "False"
                   )
            )
        ]



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
