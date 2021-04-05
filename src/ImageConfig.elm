module ImageConfig exposing (ConfigMode(..), ImageConfig, Msg(..), init, update, view)

import Basics exposing (Float, Int)
import Element exposing (Element, fillPortion, padding, px, spacing, text)
import Element.Input as Input
import Framework exposing (layout)
import Framework.Card as Card
import Framework.Grid as Grid
import Framework.Input as FrameworkInput
import Framework.Slider as Slider
import Html exposing (Html, button, div)
import Svg.Attributes exposing (strokeWidth)



-- MODEL


type ConfigMode
    = Global
    | PerCircle


type alias ImageConfig =
    { height : Int
    , width : Int
    , positionDelta : Int
    , maxCircles : Int
    , radius : Int
    , colorDelta : Int
    , globalOpacity : Float
    , opacityMode : ConfigMode
    , strokeWidth : Int
    }



-- INIT


init : () -> ImageConfig
init () =
    { height = 500
    , width = 500
    , positionDelta = 5
    , colorDelta = 5
    , maxCircles = 1000
    , radius = 5
    , globalOpacity = 1
    , strokeWidth = 1
    , opacityMode = Global
    }



-- Msg


type Msg
    = ChooseOpacityConfigMode ConfigMode
    | UpdatePositionDelta Float
    | UpdateColorDelta Float
    | UpdateMaxCircles Float
    | UpdateRadius Float
    | UpdateOpacity Float
    | UpdateStrokeWidth Float


update : Msg -> ImageConfig -> ImageConfig
update msg imageConfig =
    case msg of
        ChooseOpacityConfigMode configMode ->
            { imageConfig | opacityMode = configMode }

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
            { imageConfig | globalOpacity = value }



-- VIEW


view : ImageConfig -> Element Msg
view imageConfig =
    Element.column [ Element.height Element.fill, spacing 30, padding 10, Element.width (px 300) ]
        [ Input.slider
            (Slider.simple
                ++ [ Element.width (Element.fillPortion 2)
                   ]
            )
            { onChange = UpdatePositionDelta
            , label = Input.labelAbove FrameworkInput.label (text ("Position Delta: " ++ String.fromInt imageConfig.positionDelta))
            , min = 0
            , max = 100
            , step = Just 1
            , value = toFloat imageConfig.positionDelta
            , thumb = Input.thumb Slider.thumb
            }
        , Input.slider Slider.simple
            { onChange = UpdateColorDelta
            , label = Input.labelAbove FrameworkInput.label (text ("Color Delta: " ++ String.fromInt imageConfig.colorDelta))
            , min = 0
            , max = 30
            , step = Just 1
            , value = toFloat imageConfig.colorDelta
            , thumb = Input.thumb Slider.thumb
            }
        , Input.slider Slider.simple
            { onChange = UpdateMaxCircles
            , label = Input.labelAbove FrameworkInput.label (text ("Max Circles: " ++ String.fromInt imageConfig.maxCircles))
            , min = 1
            , max = 30001
            , step = Just 100
            , value = toFloat imageConfig.maxCircles
            , thumb = Input.thumb Slider.thumb
            }
        , Input.slider Slider.simple
            { onChange = UpdateRadius
            , label = Input.labelAbove FrameworkInput.label (text ("Radius: " ++ String.fromInt imageConfig.radius))
            , min = 1
            , max = 100
            , step = Just 1
            , value = toFloat imageConfig.radius
            , thumb = Input.thumb Slider.thumb
            }
        , Element.row
            [ padding 10
            , spacing 20
            , Element.width (px 600)
            ]
            [ Input.radioRow
                [ padding 10
                , spacing 20
                , Element.width (px 300)
                ]
                { onChange = ChooseOpacityConfigMode
                , selected = Just imageConfig.opacityMode
                , label = Input.labelAbove FrameworkInput.label (text "Opacity Mode")
                , options =
                    [ Input.option Global (text "Global")
                    , Input.option PerCircle (text "Random")
                    ]
                }
            , case imageConfig.opacityMode of
                Global ->
                    Input.slider Slider.simple
                        { onChange = UpdateOpacity
                        , label = Input.labelAbove FrameworkInput.label (text ("Opacity: " ++ String.fromFloat imageConfig.globalOpacity))
                        , min = 0
                        , max = 1
                        , step = Just 0.05
                        , value = imageConfig.globalOpacity
                        , thumb = Input.thumb Slider.thumb
                        }

                PerCircle ->
                    Element.none
            ]
        , Input.slider Slider.simple
            { onChange = UpdateStrokeWidth
            , label = Input.labelAbove FrameworkInput.label (text ("Stroke Width: " ++ String.fromInt imageConfig.strokeWidth))
            , min = 0
            , max = 100
            , step = Just 1
            , value = toFloat imageConfig.strokeWidth
            , thumb = Input.thumb Slider.thumb
            }
        ]
