module Circle exposing (Circle, CircleUpdate, ColorUpdate, ComparablePosition, InternalColor, fillColor, generate, generateCircleUpdate, opacity, updateCircle)

import Color exposing (Color)
import Direction exposing (Direction)
import ImageConfig exposing (ConfigMode(..), ImageConfig)
import Random


type alias ColorUpdate =
    { rDelta : Int, gDelta : Int, bDelta : Int }


type alias CircleUpdate =
    { direction : Direction, colorUpdate : ColorUpdate, opacity : Float }


generateColorUpdate : ImageConfig -> Random.Generator ColorUpdate
generateColorUpdate imageConfig =
    let
        low =
            -1 * imageConfig.colorDelta
    in
    let
        high =
            imageConfig.colorDelta
    in
    Random.map3
        ColorUpdate
        (Random.uniform low [ high ])
        (Random.uniform low [ high ])
        (Random.uniform low [ high ])


generateCircleUpdate : ImageConfig -> Random.Generator CircleUpdate
generateCircleUpdate imageConfig =
    Random.map3
        CircleUpdate
        (Direction.generator imageConfig.positionDelta)
        (generateColorUpdate imageConfig)
        (Random.float 0 1)


type alias Position =
    { x : Int
    , y : Int
    }


generatePosition : ImageConfig -> Random.Generator Position
generatePosition imageConfig =
    Random.map2
        Position
        (Random.int 0 imageConfig.width)
        (Random.int 0 imageConfig.height)


type alias ComparablePosition =
    ( Int, Int )


type alias InternalColor =
    { red : Int
    , green : Int
    , blue : Int
    }


internalColorToColor : InternalColor -> Color
internalColorToColor color =
    Color.rgb255 color.red color.green color.blue


internalColorToCssColor : InternalColor -> String
internalColorToCssColor color =
    internalColorToColor color
        |> Color.toCssString


internalColorGenerate : Random.Generator InternalColor
internalColorGenerate =
    Random.map3
        InternalColor
        (Random.int 0 255)
        (Random.int 0 255)
        (Random.int 0 255)


type alias Circle =
    { position : Position
    , color : InternalColor
    , radius : Int
    , opacity : Float
    }



-- UPDATE


boundedPositionUpdate : Int -> Int -> Int -> Int
boundedPositionUpdate min max value =
    let
        recBoundedPositionUpdate =
            boundedPositionUpdate min max
    in
    case compare value min of
        LT ->
            recBoundedPositionUpdate (2 * min - value)

        EQ ->
            value

        GT ->
            case compare value max of
                GT ->
                    recBoundedPositionUpdate (2 * max - value)

                LT ->
                    value

                EQ ->
                    value


updatePosition : ImageConfig -> Direction -> Position -> Position
updatePosition imageConfig direction position =
    { x = boundedPositionUpdate 0 imageConfig.width (position.x + direction.xDelta)
    , y = boundedPositionUpdate 0 imageConfig.height (position.y + direction.yDelta)
    }


updateColor : InternalColor -> ColorUpdate -> InternalColor
updateColor color colorUpdate =
    { red = clamp 0 255 (color.red + colorUpdate.rDelta)
    , green = clamp 0 255 (color.green + colorUpdate.gDelta)
    , blue = clamp 0 255 (color.blue + colorUpdate.bDelta)
    }


updateCircle : ImageConfig -> CircleUpdate -> Circle -> Circle
updateCircle imageConfig circleUpdate circle =
    { position = updatePosition imageConfig circleUpdate.direction circle.position
    , color = updateColor circle.color circleUpdate.colorUpdate
    , radius = circle.radius
    , opacity = circleUpdate.opacity
    }


fillColor : Circle -> String
fillColor circle =
    internalColorToCssColor circle.color


generate : ImageConfig -> Random.Generator Circle
generate imageConfig =
    Random.map4
        Circle
        (generatePosition imageConfig)
        internalColorGenerate
        (Random.int 1 20)
        (Random.float 0 1)


opacity : ImageConfig.ImageRenderingConfig -> Circle -> Float
opacity imageConfig circle =
    case imageConfig.opacityMode of
        Global ->
            imageConfig.globalOpacity

        PerCircle ->
            circle.opacity
