module Direction exposing (Direction, generator)

import Random


type alias Direction =
    { xDelta : Int, yDelta : Int }


deltaGenerator : Int -> Random.Generator Int
deltaGenerator maxPositionDelta =
    Random.int (-1 * maxPositionDelta) maxPositionDelta


generator : Int -> Random.Generator Direction
generator maxPositionDelta =
    Random.map2
        Direction
        (deltaGenerator maxPositionDelta)
        (deltaGenerator maxPositionDelta)
