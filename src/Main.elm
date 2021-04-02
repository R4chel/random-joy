port module Main exposing (..)

import BoundedDeque exposing (BoundedDeque)
import Browser
import Browser.Events exposing (onAnimationFrame)
import Circle exposing (Circle, CircleUpdate, ColorUpdate, ComparablePosition, InternalColor)
import Element exposing (Element, el, layout)
import Element.Input as Input
import Framework exposing (layout)
import Framework.Button as Button
import Framework.Color as FrameworkColor
import Framework.Grid as Grid
import Framework.Group as Group
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (id, src)
import ImageConfig exposing (ImageConfig, Msg(..))
import Random
import Svg exposing (circle, svg)
import Svg.Attributes exposing (color, cx, cy, fill, fillOpacity, height, r, stroke, strokeWidth, viewBox, width)



---- MODEL ----


type alias Model =
    { imageConfig : ImageConfig
    , activeCircles : List Circle
    , displayText : String
    , visibleCircles : BoundedDeque Circle
    , paused : Bool
    , stepsPerUpdate : Int
    }


init : ( Model, Cmd Msg )
init =
    let
        imageConfig =
            ImageConfig.init ()
    in
    ( { imageConfig = imageConfig
      , activeCircles = []
      , displayText = ""
      , visibleCircles = BoundedDeque.empty imageConfig.maxCircles
      , paused = False
      , stepsPerUpdate = 5
      }
    , Random.generate AddCircle (Circle.generate imageConfig)
    )



-- SUBSCRIPTIONS


port getSvg : () -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.paused || List.isEmpty model.activeCircles then
            Sub.none

          else
            onAnimationFrame (\_ -> ChooseDirection)
        ]



---- UPDATE ----


{-| TODO rename Choose\\Direction to something else now that it also includes color update
-}
type Msg
    = ChooseDirection
    | Step CircleUpdate
    | TogglePaused
    | PrintFoo
    | GetSvg
    | UpdateImageConfig ImageConfig.Msg
    | AddCircle Circle
    | GenerateNewCircle
    | Clear


step : Model -> CircleUpdate -> Model
step model circleUpdate =
    case model.activeCircles of
        [] ->
            model

        hd :: tl ->
            let
                updatedCircle =
                    Circle.updateCircle model.imageConfig circleUpdate hd
            in
            { model
                | activeCircles = tl ++ [ updatedCircle ]
                , visibleCircles =
                    BoundedDeque.pushFront
                        updatedCircle
                        model.visibleCircles
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clear ->
            ( { model
                | activeCircles = []
                , visibleCircles = BoundedDeque.empty model.imageConfig.maxCircles
              }
            , Cmd.none
            )

        Step circleUpdate ->
            ( step model circleUpdate
            , Cmd.none
            )

        ChooseDirection ->
            ( model
            , Cmd.batch
                (List.repeat model.stepsPerUpdate
                    (Random.generate Step (Circle.generateCircleUpdate model.imageConfig))
                )
            )

        PrintFoo ->
            ( { model | displayText = model.displayText ++ " HI! " }, Cmd.none )

        TogglePaused ->
            ( { model | paused = not model.paused }
            , Cmd.none
            )

        GetSvg ->
            ( model, getSvg () )

        UpdateImageConfig ((ImageConfig.UpdateMaxCircles value) as imageConfigUpdate) ->
            ( { model
                | imageConfig =
                    ImageConfig.update imageConfigUpdate
                        model.imageConfig
                , visibleCircles = BoundedDeque.resize (\_ -> round value) model.visibleCircles
              }
            , Cmd.none
            )

        UpdateImageConfig imageConfigUpdate ->
            ( { model
                | imageConfig =
                    ImageConfig.update imageConfigUpdate
                        model.imageConfig
              }
            , Cmd.none
            )

        AddCircle circle ->
            ( { model
                | activeCircles = model.activeCircles ++ [ circle ]
                , visibleCircles = BoundedDeque.pushFront circle model.visibleCircles
              }
            , Cmd.none
            )

        GenerateNewCircle ->
            ( model, Random.generate AddCircle (Circle.generate model.imageConfig) )



---- VIEW ----


view : Model -> Html Msg
view model =
    Framework.layout [] <|
        Element.el Framework.container <|
            Element.column [ Element.spacing 5 ]
                [ Element.row [ Element.spacing 5 ]
                    (buttonsView
                        model
                    )
                , Element.row [ Element.spacing 10 ]
                    [ artView model, controlPanelView model ]
                ]


artView : Model -> Element Msg
artView model =
    Element.column [ Element.height Element.fill, Element.width Element.fill ] [ Element.html (modelToSvg model) ]


controlPanelView : Model -> Element Msg
controlPanelView model =
    Element.column
        [ Element.height Element.fill ]
        [ ImageConfig.view model.imageConfig |> Element.map (\x -> UpdateImageConfig x) ]


buttonsView : Model -> List (Element Msg)
buttonsView model =
    let
        buttonStyle =
            Button.simple ++ FrameworkColor.primary
    in
    [ Input.button buttonStyle <|
        { onPress = Just TogglePaused
        , label =
            Element.text
                (if model.paused then
                    "Play"

                 else
                    "Pause"
                )
        }
    , Input.button buttonStyle <|
        { onPress = Just Clear
        , label = Element.text "Clear"
        }
    , Input.button buttonStyle <|
        { onPress = Just GenerateNewCircle
        , label = Element.text "+"
        }
    , Input.button buttonStyle <|
        { onPress = Just GetSvg
        , label = Element.text "Download"
        }
    ]


viewCircle : ImageConfig -> Circle -> Svg.Svg msg
viewCircle imageConfig c =
    circle
        [ cx (String.fromInt c.position.x)
        , cy (String.fromInt c.position.y)
        , r (String.fromInt imageConfig.radius)
        , fill (Circle.fillColor c)
        , stroke (Circle.fillColor c)
        , fillOpacity (String.fromFloat (Circle.opacity imageConfig c))
        , strokeWidth (String.fromInt imageConfig.strokeWidth)
        ]
        []


pixels model =
    BoundedDeque.takeBack model.imageConfig.maxCircles model.visibleCircles |> List.map (viewCircle model.imageConfig)


modelToSvg model =
    svg
        [ id "output"
        , width (String.fromInt model.imageConfig.width)
        , height (String.fromInt model.imageConfig.height)
        , viewBox (String.join " " [ "0", "0", String.fromInt model.imageConfig.width, String.fromInt model.imageConfig.height ])
        ]
        (pixels model)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
