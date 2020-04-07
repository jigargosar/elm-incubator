module Main2 exposing (main)

import Basics.Extra exposing (flip, uncurry)
import Dict exposing (Dict)
import Playground exposing (..)



-- Game Scaffold


type Mem
    = Mem (Maybe Computer) GridCells


type alias GridCells =
    Dict ( Int, Int ) Anim


initialGridCells : GridCells
initialGridCells =
    gridPositions
        |> List.map (flip Tuple.pair (static 1))
        |> Dict.fromList


static val =
    Anim { from = val, to = val, duration = 0, elapsed = 0 }


type Anim
    = Anim { from : Number, to : Number, duration : Number, elapsed : Number }


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



--currentProgress (Anim sc) =
--    if sc.from == sc.to || sc.duration <= 0 || sc.elapsed >= sc.duration then
--        1
--
--    else
--        sc.elapsed / sc.duration
--


currentValue (Anim sc) =
    if sc.from == sc.to || sc.duration <= 0 || sc.elapsed >= sc.duration then
        sc.to

    else
        let
            progress =
                sc.elapsed / sc.duration

            v =
                (sc.to - sc.from) * progress + sc.from
        in
        v


retargetAnim : Number -> Anim -> Anim
retargetAnim to ((Anim a) as an) =
    let
        nv =
            { a | from = a.to, to = to, elapsed = 0 }
    in
    Anim nv


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
                Anim sa ->
                    Anim { sa | elapsed = sa.elapsed + 1 }
    in
    Dict.map
        (\gIdx anim ->
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
                    tickCellAnimation anim

                MouseEnter ->
                    retargetAnim 0.5 anim

                --Anim { from = 1, to = 0.5, duration = 30, elapsed = 0 }
                MouseLeave ->
                    retargetAnim 1 anim

                --Anim { from = 0.5, to = 1, duration = 30, elapsed = 0 }
                MouseOver ->
                    tickCellAnimation anim
        )
        gridCells
        |> Mem (Just computer)


view : Computer -> Mem -> List Shape
view _ (Mem _ gridCells) =
    [ group (List.map renderWaterCell (Dict.toList gridCells))
    ]


renderWaterCell : ( ( Int, Int ), Anim ) -> Shape
renderWaterCell ( gIdx, cellAnimation ) =
    let
        _ =
            currentValue cellAnimation
    in
    circle lightBlue waterRadius
        |> moveGridIdxToScreen gIdx
        |> scale (currentValue cellAnimation)


waterRadius =
    gridCellWidth * 0.4


moveGridIdxToScreen gIdx =
    uncurry move (gIdxToScreen gIdx)


main =
    game view update init
