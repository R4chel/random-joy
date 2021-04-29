module InternalColor exposing (ColorUpdate, InternalColor, generate, generateColorUpdate, toCssColor)

import Color exposing (Color)
import ImageConfig exposing (ConfigMode(..), ImageConfig)
import Random


type alias ColorUpdate =
    { rDelta : Int, gDelta : Int, bDelta : Int }


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


type alias InternalColor =
    { red : Int
    , green : Int
    , blue : Int
    }


toColor : InternalColor -> Color
toColor color =
    Color.rgb255 color.red color.green color.blue


toCssColor : InternalColor -> String
toCssColor color =
    toColor color
        |> Color.toCssString


generate : Random.Generator InternalColor
generate =
    Random.map3
        InternalColor
        (Random.int 0 255)
        (Random.int 0 255)
        (Random.int 0 255)
