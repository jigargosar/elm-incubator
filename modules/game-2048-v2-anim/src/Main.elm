module Main exposing (main)

import Browser exposing (Document)
import Cons exposing (Cons)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Keyed
import IntPos exposing (IntPos)
import Process
import Set
import Task



-- Model


type alias Model =
    { gridCons : Cons GridModel
    , gridViewModel : GridViewModel
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { gridCons = initialGridModelCons
      , gridViewModel =
            Cons.head initialGridModelCons
                |> initGridViewModel
      }
    , stepTiles
    )


stepTiles =
    Process.sleep 1500 |> Task.perform (always StepTiles)



-- Update


type Msg
    = NoOp
    | StepTiles


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        StepTiles ->
            case Cons.fromTail model.gridCons of
                Nothing ->
                    init ()

                Just gridCons ->
                    ( { model
                        | gridCons = gridCons
                        , gridViewModel = updateGridViewModel (Cons.head gridCons) model.gridViewModel
                      }
                    , stepTiles
                    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


type alias DM =
    Document Msg


type alias HM =
    Html Msg


view : Model -> DM
view model =
    Document "2048 Animated"
        [ div [ class "pa3 measure center" ]
            [ div [ class "pa3 f3" ] [ text "Play 2048" ]
            , viewGridViewModel model.gridViewModel
            ]
        ]



-- Tile


type alias Tile =
    { id : String
    , num : Int
    , pos : IntPos
    , foo : TileFoo
    }


type TileFoo
    = Generated
    | Merged
    | None
    | ReadyForRemoval


type TileView
    = TileVisible Tile
    | TileToBeRemoved String


idOfVisibleTileView : TileView -> Maybe String
idOfVisibleTileView tileView =
    case tileView of
        TileVisible t ->
            Just t.id

        TileToBeRemoved _ ->
            Nothing


idOfTileView : TileView -> String
idOfTileView tileView =
    case tileView of
        TileVisible t ->
            t.id

        TileToBeRemoved id ->
            id


type alias GridViewModel =
    List TileView


initGridViewModel : GridModel -> GridViewModel
initGridViewModel =
    List.map TileVisible


updateGridViewModel : GridModel -> GridViewModel -> GridViewModel
updateGridViewModel newTileList oldTileViewList =
    let
        newTileViewList =
            List.map TileVisible newTileList

        oldTileIdSet =
            Set.fromList (List.filterMap idOfVisibleTileView oldTileViewList)

        newTileIdSet =
            Set.fromList (List.filterMap idOfVisibleTileView newTileViewList)

        removedTileIdSet =
            Set.diff oldTileIdSet newTileIdSet

        toBeRemovedTileViewList : List TileView
        toBeRemovedTileViewList =
            removedTileIdSet
                |> Set.toList
                |> List.map TileToBeRemoved

        finalTileViewList =
            newTileViewList
                ++ toBeRemovedTileViewList
                |> List.sortBy idOfTileView
    in
    finalTileViewList



-- Grid


type alias GridModel =
    List Tile


gridFromTiles : List Tile -> GridModel
gridFromTiles =
    identity


initialGridModelCons : Cons GridModel
initialGridModelCons =
    let
        initialTileList : GridModel
        initialTileList =
            gridFromTiles
                [ Tile "a" 2 ( 1, 1 ) None
                , Tile "b" 4 ( 2, 2 ) None
                ]

        restTileList : List GridModel
        restTileList =
            List.map gridFromTiles
                [ -- Right
                  [ Tile "a" 2 ( 3, 1 ) None
                  , Tile "b" 4 ( 3, 2 ) None
                  , Tile "c" 2 ( 2, 1 ) Generated
                  ]
                , -- Left
                  [ Tile "a" 2 ( 0, 1 ) None
                  , Tile "b" 4 ( 0, 2 ) None
                  , Tile "c" 2 ( 0, 1 ) None
                  , Tile "d" 4 ( 0, 1 ) Merged
                  , Tile "e" 2 ( 1, 1 ) Generated
                  ]
                , -- Up
                  [ Tile "b" 4 ( 0, 0 ) None
                  , Tile "d" 4 ( 0, 0 ) None
                  , Tile "e" 2 ( 1, 0 ) None
                  , Tile "f" 8 ( 0, 0 ) Merged
                  , Tile "g" 4 ( 1, 1 ) Generated
                  ]

                -- Right
                , [ Tile "e" 2 ( 3, 0 ) None
                  , Tile "f" 8 ( 2, 0 ) None
                  , Tile "g" 4 ( 3, 1 ) None
                  ]

                -- Clear
                , []
                ]
    in
    Cons.init initialTileList restTileList


viewGridViewModel : GridViewModel -> HM
viewGridViewModel tiles =
    div
        [ class "pa3 code f2 debug"
        ]
        [ Html.Keyed.node "div"
            [ style "width" "400px"
            , style "height" "400px"
            ]
            (List.map renderKeyedTileView tiles)
        ]


renderKeyedTileView : TileView -> ( String, HM )
renderKeyedTileView tileView =
    case tileView of
        TileVisible tile ->
            renderKeyedTile tile

        TileToBeRemoved id ->
            ( id, text "" )

renderKeyedTile: Tile -> ( String, HM )
renderKeyedTile tile =
    ( tile.id, if tile.foo == ReadyForRemoval then text "" else  viewTile tile )


viewTile : Tile -> HM
viewTile tile =
    div
        [ style "width" "100px"
        , style "height" "100px"
        , class "absolute flex justify-center items-center"
        , style "transform" (renderTileTransform tile)
        , style "transition" "transform 750ms"
        , style "outline" "none"
        ]
        [ div
            [ style "width" "100px"
            , style "height" "100px"
            , style "background-color" "rgba(255,255,255,0.9)"
            , class "flex justify-center items-center"
            , case tile.foo of
                Generated ->
                    class "animate__animated  animate__zoomIn animate__delay-2s "

                Merged ->
                    class "animate__animated  animate__bounceIn "

                None ->
                    class ""


                ReadyForRemoval ->
                    class ""

                                ]
            [ text (String.fromInt tile.num) ]
        ]


renderTileTransform tile =
    let
        postPartToPx n =
            String.fromInt (n * 100) ++ "px"

        ( sx, sy ) =
            tile.pos
                |> Tuple.mapBoth postPartToPx postPartToPx
    in
    [ "translate(", sx, ",", sy, ")" ]
        |> String.join " "



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
