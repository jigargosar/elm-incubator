module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Json.Decode as D exposing (Decoder)
import Random exposing (Generator, Seed)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes exposing (fill, stroke, strokeWidth)
import TypedSvg.Attributes exposing (transform)
import TypedSvg.Attributes.InPx
import TypedSvg.Types exposing (Transform(..))



-- Input


type alias Input =
    { leftDown : Bool, rightDown : Bool, keys : Set String }


recordKey : String -> Bool -> Input -> Input
recordKey key isDown input =
    let
        keys =
            if isDown then
                Set.insert key input.keys

            else
                Set.remove key input.keys
    in
    { leftDown = Set.member "ArrowLeft" keys
    , rightDown = Set.member "ArrowRight" keys
    , keys = keys
    }


initialInput : Input
initialInput =
    Input False False Set.empty



-- Vec


type alias Vec =
    { x : Float, y : Float }


newVec : Float -> Float -> Vec
newVec x y =
    Vec x y


vecFromRTheta : Float -> Float -> Vec
vecFromRTheta radius theta =
    let
        ( x, y ) =
            fromPolar ( radius, theta )
    in
    newVec x y


addVec : Vec -> Vec -> Vec
addVec a b =
    newVec (a.x + b.x) (a.y + b.y)


mapY : (Float -> Float) -> Vec -> Vec
mapY func vec =
    newVec vec.x (func vec.y)



--scaleVec : Float -> Vec -> Vec
--scaleVec float vec =
--    newVec (vec.x * float) (vec.y * float)
--


constrainVecInBounds : Bounds -> Vec -> Vec
constrainVecInBounds bounds vec =
    newVec (clamp bounds.min.x bounds.max.x vec.x) (clamp bounds.min.y bounds.max.y vec.y)



-- SIZE


type alias Size =
    { width : Float, height : Float }


newSize : Float -> Float -> Size
newSize w h =
    Size w h


shrinkSizeBy : Size -> Size -> Size
shrinkSizeBy b a =
    newSize (a.width - b.width) (a.height - b.height)


shrinkSizeByRadius : Float -> Size -> Size
shrinkSizeByRadius radius a =
    newSize (a.width - (radius * 2)) (a.height - (radius * 2))


growSizeByRadius : Float -> Size -> Size
growSizeByRadius radius =
    shrinkSizeByRadius -radius


viewBoxOfSize : Size -> Svg.Attribute msg
viewBoxOfSize size =
    TypedSvg.Attributes.viewBox
        (-size.width / 2)
        (-size.height / 2)
        size.width
        size.height



-- Bounds


type alias Bounds =
    { min : Vec
    , max : Vec
    }


newBoundsAtOrigin : Size -> Bounds
newBoundsAtOrigin size =
    { min = newVec (size.width * -0.5) (size.height * -0.5)
    , max = newVec (size.width * 0.5) (size.height * 0.5)
    }


shiftBounds : Vec -> Bounds -> Bounds
shiftBounds vec bounds =
    { bounds
        | min = addVec bounds.min vec
        , max = addVec bounds.max vec
    }


boundsContain : Vec -> Bounds -> Bool
boundsContain vec bounds =
    (clamp bounds.min.x bounds.max.x vec.x == vec.x)
        && (clamp bounds.min.y bounds.max.y vec.y == vec.y)



-- Paddle


type alias Paddle =
    { pos : Vec
    , size : Size
    }


initPaddle : Size -> Paddle
initPaddle canvasSize =
    let
        size =
            newSize 80 10

        pos =
            newVec 0 ((canvasSize.height * 0.5) - 20)
    in
    Paddle pos size


updatePaddle : Size -> Input -> Paddle -> Paddle
updatePaddle canvasSize input paddle =
    let
        xSpeed =
            10

        dxLeft =
            if input.leftDown then
                -xSpeed

            else
                0

        dxRight =
            if input.rightDown then
                xSpeed

            else
                0

        velocity =
            newVec (dxLeft + dxRight) 0

        paddleBoundary =
            shrinkSizeBy paddle.size canvasSize
                |> newBoundsAtOrigin
    in
    { paddle
        | pos =
            addVec paddle.pos velocity
                |> constrainVecInBounds paddleBoundary
    }


viewPaddle : Paddle -> Svg msg
viewPaddle paddle =
    rect
        paddle.size.width
        paddle.size.height
        [ transform [ Translate paddle.pos.x paddle.pos.y ]
        ]



-- Ball


type alias Ball =
    { pos : Vec
    , radius : Float
    , vel : Vec
    }


initBall : Size -> Ball
initBall _ =
    let
        pos =
            newVec 0 0

        radius =
            15

        speed =
            4

        angle =
            degrees 45
    in
    { pos = pos
    , radius = radius
    , vel = vecFromRTheta speed angle
    }


bounceBallOnPaddleTop : Paddle -> Ball -> Ball
bounceBallOnPaddleTop paddle ball =
    let
        paddleBounds =
            growSizeByRadius ball.radius paddle.size
                |> newBoundsAtOrigin
                |> shiftBounds paddle.pos

        dyFunc =
            if boundsContain ball.pos paddleBounds && ball.vel.y > 0 then
                negate

            else
                identity

        newVel =
            mapY dyFunc ball.vel
    in
    { ball | vel = newVel }


updateBallPosition : Ball -> Ball
updateBallPosition ball =
    { ball | pos = addVec ball.pos ball.vel }


bounceBallWithInCanvasEdges : Size -> Ball -> Ball
bounceBallWithInCanvasEdges canvasSize ball =
    let
        ballBoundary =
            shrinkSizeByRadius ball.radius canvasSize
                |> newBoundsAtOrigin

        dx =
            if
                (ball.pos.x < ballBoundary.min.x && ball.vel.x < 0)
                    || (ball.pos.x > ballBoundary.max.x && ball.vel.x > 0)
            then
                negate ball.vel.x

            else
                ball.vel.x

        dy =
            if
                (ball.pos.y < ballBoundary.min.y && ball.vel.y < 0)
                    || (ball.pos.y > ballBoundary.max.y && ball.vel.y > 0)
            then
                negate ball.vel.y

            else
                ball.vel.y
    in
    { ball | vel = newVec dx dy }


viewBall : Ball -> Svg msg
viewBall ball =
    Svg.circle
        [ TypedSvg.Attributes.InPx.r ball.radius
        , transform [ Translate ball.pos.x ball.pos.y ]
        ]
        []



-- Brick


type alias Brick =
    { pos : Vec
    , radius : Float
    }


brickGenerator : Size -> Generator Brick
brickGenerator canvasSize =
    let
        radius =
            Random.float 10 20

        brickBounds =
            shrinkSizeBy (newSize 50 150) canvasSize
                |> newBoundsAtOrigin
                |> shiftBounds (newVec 0 -50)

        pos =
            Random.map2 newVec
                (Random.float brickBounds.min.x brickBounds.max.x)
                (Random.float brickBounds.min.y brickBounds.max.y)
    in
    Random.map2 Brick pos radius


viewBrick : Brick -> Svg msg
viewBrick brick =
    Svg.circle
        [ TypedSvg.Attributes.InPx.r brick.radius
        , transform [ Translate brick.pos.x brick.pos.y ]
        ]
        []



-- Model


type alias Model =
    { input : Input
    , paddle : Paddle
    , ball : Ball
    , bricks : List Brick
    , canvasSize : Size
    , seed : Seed
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    let
        canvasSize =
            newSize 600 300

        seed0 =
            Random.initialSeed 0

        ( bricks, seed1 ) =
            Random.step (Random.list 10 (brickGenerator canvasSize)) seed0
    in
    ( { input = initialInput
      , paddle = initPaddle canvasSize
      , ball = initBall canvasSize
      , bricks = bricks
      , canvasSize = canvasSize
      , seed = seed1
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | OnKeyDown String
    | OnKeyUp String
    | Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        OnKeyDown key ->
            ( { model | input = recordKey key True model.input }, Cmd.none )

        OnKeyUp key ->
            ( { model | input = recordKey key False model.input }, Cmd.none )

        Tick ->
            ( { model
                | paddle = updatePaddle model.canvasSize model.input model.paddle
                , ball =
                    updateBallPosition model.ball
                        |> bounceBallWithInCanvasEdges model.canvasSize
                        |> bounceBallOnPaddleTop model.paddle
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown (D.field "key" D.string |> D.map OnKeyDown)
        , Browser.Events.onKeyUp (D.field "key" D.string |> D.map OnKeyUp)
        , Browser.Events.onAnimationFrameDelta (always Tick)
        ]



-- View


type alias DM =
    Document Msg


view : Model -> DM
view model =
    let
        canvasSize =
            model.canvasSize
    in
    Document "Brick Breaker"
        [ Svg.svg
            [ viewBoxOfSize canvasSize
            , fill "none"
            , stroke "gray"
            , strokeWidth "1"
            ]
            [ rect canvasSize.width canvasSize.height []
            , viewPaddle model.paddle
            , viewBall model.ball
            , Svg.g [] (List.map viewBrick model.bricks)
            ]
        ]



-- DRAW


rect w h attrs =
    Svg.rect
        ([ TypedSvg.Attributes.InPx.x (-w / 2)
         , TypedSvg.Attributes.InPx.y (-h / 2)
         , TypedSvg.Attributes.InPx.width w
         , TypedSvg.Attributes.InPx.height h
         ]
            ++ attrs
        )
        []



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
