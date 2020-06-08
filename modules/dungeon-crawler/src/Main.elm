module Main exposing (main)

import Basics.More exposing (..)
import Browser
import Browser.Events
import Dimension exposing (Dimension)
import Html exposing (Html, div, text)
import Json.Decode as JD
import Location exposing (Location)
import Random exposing (Generator, Seed)
import Styles as S
import Svg
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
    div [ class "fixed absolute--fill" ]
        [ div [ class "pv3 f3" ] [ text "Dungeon Crawler" ]
        , viewWorld model
        ]


viewWorld model =
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
                                    |> scale 1
                            )
                   )
    in
    renderDrawing
        { dimension = dimension
        , cellSize =
            Tuple.repeatFloat 32
        }
        pictures



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
                    |> scale 5
            )



-- Drawing


type alias GridConfig =
    { dimension : Dimension
    , cellSize : Float2
    , origin : Float2
    }


type alias Config =
    { dimension : Dimension
    , cellSize : Float2
    }


toGridSize : Config -> Float2
toGridSize c =
    let
        ( gw, gh ) =
            Dimension.toFloat c.dimension

        ( cw, ch ) =
            c.cellSize
    in
    ( gw * cw, gh * ch )


toCellPosition : Config -> Location -> Float2
toCellPosition c l =
    let
        ( x, y ) =
            Location.toFloat l

        ( cw, ch ) =
            c.cellSize
    in
    ( x * cw, y * ch )


renderDrawing : Config -> List Picture -> Html msg
renderDrawing c pictures =
    let
        ( gwPx, ghPx ) =
            toGridSize c
    in
    div
        [ S.width gwPx
        , S.height ghPx
        , class "relative"
        ]
        (pictures |> List.map (renderPicture c))



-- Picture


type Picture
    = TextCell Location String Float


commonStyles =
    class "flex items-center justify-center absolute top-0 left-0"


renderPicture : Config -> Picture -> Html msg
renderPicture config picture =
    case picture of
        TextCell location displayText scaleNum ->
            let
                dxy =
                    toCellPosition config location

                ( w, h ) =
                    config.cellSize
            in
            div
                [ commonStyles
                , S.width w
                , S.height h
                , S.transforms [ S.translate dxy, S.scale scaleNum ]
                ]
                [ text displayText ]


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
