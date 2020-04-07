module Main2 exposing (main)

import Basics.Extra exposing (flip, uncurry)
import Dict exposing (Dict)
import Playground exposing (..)



-- Animation


type Anim
    = Anim
        { from : Number
        , to : Number
        , duration : Number
        , elapsed : Number
        }


animStatic : Number -> Anim
animStatic val =
    Anim { from = val, to = val, duration = 10, elapsed = 0 }


animProgress : Anim -> Float
animProgress (Anim sc) =
    if sc.from == sc.to || sc.duration <= 0 || sc.elapsed >= sc.duration then
        1

    else
        sc.elapsed / sc.duration


animTick : Anim -> Anim
animTick (Anim sa) =
    Anim { sa | elapsed = sa.elapsed + 1 }


animValue : Anim -> Number
animValue ((Anim sc) as anim) =
    (sc.to - sc.from) * animProgress anim + sc.from


animRetarget : Number -> Anim -> Anim
animRetarget to ((Anim a) as an) =
    let
        nv =
            { a | from = animValue an, to = to, elapsed = 0 }
    in
    Anim nv



-- Game Scaffold


type Mem
    = Mem (Maybe Computer) GridCells


type alias GridCells =
    Dict ( Int, Int ) Anim


initialGridCells : GridCells
initialGridCells =
    gridPositions
        |> List.map (flip Tuple.pair (animStatic 1))
        |> Dict.fromList


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
                    animTick anim

                MouseEnter ->
                    animRetarget 0.5 anim

                MouseLeave ->
                    animRetarget 1 anim

                MouseOver ->
                    animTick anim
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
            animValue cellAnimation
    in
    circle lightBlue waterRadius
        |> moveGridIdxToScreen gIdx
        |> scale (animValue cellAnimation)


waterRadius =
    gridCellWidth * 0.4


moveGridIdxToScreen gIdx =
    uncurry move (gIdxToScreen gIdx)


main =
    game view update init
