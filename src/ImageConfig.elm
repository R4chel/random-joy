module ImageConfig exposing (ImageConfig, Msg(..), init, update, view)

import Basics exposing (Float, Int)
import Element exposing (Element, text)
import Element.Input as Input
import Framework exposing (layout)
import Framework.Slider as Slider
import Html exposing (Html, button, div)
import Svg.Attributes exposing (strokeWidth)



-- MODEL


type alias ImageConfig =
    { height : Int
    , width : Int
    , positionDelta : Int
    , maxCircles : Int
    , radius : Int
    , colorDelta : Int
    , opacity : Float
    , strokeWidth : Int
    }



-- INIT
-- TODO: don't need both slider and values in config, slider contains value


init : () -> ImageConfig
init () =
    { height = 500
    , width = 500
    , positionDelta = 5
    , colorDelta = 5
    , maxCircles = 1000
    , radius = 5
    , opacity = 1
    , strokeWidth = 1
    }



-- Msg


type Msg
    = UpdatePositionDelta Float
    | UpdateColorDelta Float
    | UpdateMaxCircles Float
    | UpdateRadius Float
    | UpdateOpacity Float
    | UpdateStrokeWidth Float


update : Msg -> ImageConfig -> ImageConfig
update msg imageConfig =
    case msg of
        UpdatePositionDelta value ->
            { imageConfig | positionDelta = round value }

        UpdateColorDelta value ->
            { imageConfig | colorDelta = round value }

        UpdateMaxCircles value ->
            { imageConfig | maxCircles = round value }

        UpdateRadius value ->
            { imageConfig | radius = round value }

        UpdateStrokeWidth value ->
            { imageConfig | strokeWidth = round value }

        UpdateOpacity value ->
            { imageConfig | opacity = value }



-- VIEW


view : ImageConfig -> Element Msg
view imageConfig =
    Element.column [ Element.height Element.fill ]
        [ Input.slider Slider.simple
            { onChange = UpdatePositionDelta
            , label = Input.labelAbove [] (text ("Position Delta: " ++ String.fromInt imageConfig.positionDelta))
            , min = 0
            , max = 100
            , step = Just 1
            , value = toFloat imageConfig.positionDelta
            , thumb = Input.defaultThumb
            }
        , Input.slider Slider.simple
            { onChange = UpdateColorDelta
            , label = Input.labelAbove [] (text ("Color Delta: " ++ String.fromInt imageConfig.colorDelta))
            , min = 0
            , max = 30
            , step = Just 1
            , value = toFloat imageConfig.colorDelta
            , thumb = Input.defaultThumb
            }
        , Input.slider Slider.simple
            { onChange = UpdateMaxCircles
            , label = Input.labelAbove [] (text ("Max Circles: " ++ String.fromInt imageConfig.maxCircles))
            , min = 1
            , max = 10000
            , step = Just 100
            , value = toFloat imageConfig.maxCircles
            , thumb = Input.defaultThumb
            }
        , Input.slider Slider.simple
            { onChange = UpdateRadius
            , label = Input.labelAbove [] (text ("Radius: " ++ String.fromInt imageConfig.radius))
            , min = 1
            , max = 100
            , step = Just 1
            , value = toFloat imageConfig.radius
            , thumb = Input.defaultThumb
            }
        , Input.slider Slider.simple
            { onChange = UpdateOpacity
            , label = Input.labelAbove [] (text ("Opacity: " ++ String.fromFloat imageConfig.opacity))
            , min = 0
            , max = 1
            , step = Just 0.05
            , value = imageConfig.opacity
            , thumb = Input.defaultThumb
            }
        , Input.slider Slider.simple
            { onChange = UpdateStrokeWidth
            , label = Input.labelAbove [] (text ("Stroke Width: " ++ String.fromInt imageConfig.strokeWidth))
            , min = 0
            , max = 100
            , step = Just 1
            , value = toFloat imageConfig.strokeWidth
            , thumb = Input.defaultThumb
            }
        ]
