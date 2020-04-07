module Main2 exposing (main)

import Basics.Extra exposing (flip, uncurry)
import Dict exposing (Dict)
import Playground exposing (..)



-- Game Scaffold


type Mem
    = Mem (Maybe Computer) GridCells


type alias GridCells =
    Dict ( Int, Int ) CellAnimation


initialGridCells : GridCells
initialGridCells =
    gridPositions
        |> List.map (flip Tuple.pair (static 1))
        |> Dict.fromList


static val =
    Scaling { from = val, to = val, duration = 0, elapsed = 0 }


type CellAnimation
    = Scaling { from : Number, to : Number, duration : Number, elapsed : Number }


type Event
    = MouseEnter
    | MouseLeave
    | MouseOver
    | NoEvent


init : Mem
init =
    Mem Nothing initialGridCells


gridXLength =
    7


gridYLength =
    7


gridCellWidth =
    70


gridXYLeftTop : ( Float, Float )
gridXYLeftTop =
    ( (toFloat gridXLength * gridCellWidth - gridCellWidth) * -0.5
    , (toFloat gridYLength * gridCellWidth - gridCellWidth) * -0.5
    )


gIdxToScreen : ( Int, Int ) -> ( Float, Float )
gIdxToScreen ( x, y ) =
    let
        ( dx, dy ) =
            gridXYLeftTop
    in
    ( toFloat x * gridCellWidth + dx, toFloat y * gridCellWidth + dy )


isGIdxValid : ( Int, Int ) -> Bool
isGIdxValid ( x, y ) =
    x >= 0 && x <= gridXLength && y >= 0 && y < gridYLength


validGIdx : ( Int, Int ) -> Maybe ( Int, Int )
validGIdx gIdx =
    if isGIdxValid gIdx then
        Just gIdx

    else
        Nothing


screenToGIdx : Float -> Float -> Maybe ( Int, Int )
screenToGIdx x y =
    let
        ( dx, dy ) =
            gridXYLeftTop
    in
    ( round ((x - dx) / gridCellWidth), round ((y - dy) / gridCellWidth) )
        |> validGIdx


computerToGIdx : Computer -> Maybe ( Int, Int )
computerToGIdx { mouse } =
    screenToGIdx mouse.x mouse.y


gridPositions : List ( Int, Int )
gridPositions =
    List.range 0 (gridXLength - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (gridYLength - 1) |> List.map (Tuple.pair x)
            )


update : Computer -> Mem -> Mem
update computer (Mem maybePreviousComputer gridCells) =
    let
        maybeCurrentMouseGIdx : Maybe ( Int, Int )
        maybeCurrentMouseGIdx =
            computerToGIdx computer

        maybePreviousMouseGIdx : Maybe ( Int, Int )
        maybePreviousMouseGIdx =
            maybePreviousComputer |> Maybe.andThen computerToGIdx

        tickCellAnimation cellAnimation =
            case cellAnimation of
                Scaling sa ->
                    Scaling { sa | elapsed = sa.elapsed + 1 }
    in
    Dict.map
        (\gIdx cellAnimation ->
            let
                event =
                    if maybeCurrentMouseGIdx == Just gIdx then
                        if maybePreviousMouseGIdx == Just gIdx then
                            MouseOver

                        else
                            MouseEnter

                    else if maybePreviousMouseGIdx == Just gIdx then
                        MouseLeave

                    else
                        NoEvent
            in
            case event of
                NoEvent ->
                    tickCellAnimation cellAnimation

                MouseEnter ->
                    Scaling { from = 1, to = 0.5, duration = 30, elapsed = 0 }

                MouseLeave ->
                    Scaling { from = 0.5, to = 1, duration = 30, elapsed = 0 }

                MouseOver ->
                    tickCellAnimation cellAnimation
        )
        gridCells
        |> Mem (Just computer)


view : Computer -> Mem -> List Shape
view _ (Mem _ gridCells) =
    [ group (List.map renderWaterCell (Dict.toList gridCells))
    ]


renderWaterCell : ( ( Int, Int ), CellAnimation ) -> Shape
renderWaterCell ( gIdx, cellAnimation ) =
    let
        currentValue (Scaling sc) =
            if sc.from == sc.to || sc.duration <= 0 || sc.elapsed >= sc.duration then
                sc.to

            else
                let
                    progress =
                        sc.elapsed / sc.duration
                in
                Debug.log "debug" progress

        _ =
            currentValue cellAnimation
    in
    circle lightBlue waterRadius
        |> moveGridIdxToScreen gIdx
        |> scale
            (case cellAnimation of
                Scaling sa ->
                    sa.to
            )


waterRadius =
    gridCellWidth * 0.4


moveGridIdxToScreen gIdx =
    uncurry move (gIdxToScreen gIdx)


main =
    game view update init
