module Main exposing (main)

import Basics.More exposing (..)
import Browser
import Browser.Events
import Dimension exposing (Dimension)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import HtmlStyle as HS
import Json.Decode as JD
import Location exposing (Location)
import Random exposing (Generator, Seed)
import Tuple.More as Tuple



-- Model


type alias Model =
    { dimension : Dimension
    , seed : Seed
    }


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        dimension =
            Dimension.new 12 16

        initialSeed =
            Random.initialSeed (flags.now |> always 4)
    in
    ( { dimension = dimension
      , seed = initialSeed
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | KeyDown String
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        KeyDown _ ->
            ( model
            , Cmd.none
            )

        Tick _ ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown
            (JD.field "key" JD.string
                |> JD.map KeyDown
            )
        , subscribeIf False (Browser.Events.onAnimationFrameDelta Tick)
        ]



-- View


type alias HM =
    Html Msg


view : Model -> Html Msg
view model =
    div [ class "measure center" ]
        [ div [ class "pv3 f3" ] [ text "Dungeon Crawler" ]
        , div [ class "flex relative" ]
            [ viewGrid model
            ]
        ]


viewGrid : Model -> HM
viewGrid model =
    let
        dimension =
            model.dimension

        pictures =
            backgroundTiles dimension []
                ++ (dimension
                        |> Dimension.toLocations
                        |> List.map
                            (\l ->
                                words (l |> Location.toTuple |> Tuple.fromInt |> Tuple.join ",")
                                    |> atLocation l
                                    |> scale 0.3
                            )
                   )
    in
    div [ class "center code f2 bg-black white pa3 br3" ]
        [ renderDrawing { dimension = dimension, cellSize = ( 32, 32 ) } pictures
        ]



--viewOverlayMsg progress message =
--    div
--        [ class "absolute w-100 h-100 flex items-center justify-center"
--        , HS.opacity progress
--        ]
--        [ div [ class "bg-white-50 black pa3 br3" ]
--            [ div [ class "code b f2 tc" ] [ text message ]
--            , div [ class "code b f3 tc" ] [ text "Ctrl+R to restart" ]
--            ]
--        ]


backgroundTiles : Dimension -> List Location -> List Picture
backgroundTiles dimension walls =
    Dimension.toLocations dimension
        |> List.map
            (\location ->
                (if List.member location walls then
                    words "#"

                 else
                    words "."
                )
                    |> atLocation location
            )



-- Drawing


renderDrawing c pictures =
    let
        ( gw, gh ) =
            Dimension.toFloat c.dimension

        ( cw, ch ) =
            c.cellSize

        ( gwPx, ghPx ) =
            ( gw * cw, gh * ch )
    in
    div
        [ HS.width gwPx
        , HS.height ghPx
        , class "relative"
        ]
        (pictures |> List.map renderPicture)



-- Picture


type Picture
    = TextCell Location String Float


commonStyles =
    class "w2 h2 flex items-center justify-center absolute top-0 left-0 bg-black-50"


locationToDXY location =
    Location.toTuple location
        |> Tuple.toFloatScaled 32


renderPicture : Picture -> Html msg
renderPicture picture =
    case picture of
        TextCell l t s ->
            div [ commonStyles, HS.transforms [ HS.move (locationToDXY l), HS.scale s ] ]
                [ text t ]


words : String -> Picture
words t =
    TextCell Location.zero t 1


atLocation l p =
    case p of
        TextCell _ w s ->
            TextCell l w s


scale ns p =
    case p of
        TextCell l w s ->
            TextCell l w (ns * s)



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
