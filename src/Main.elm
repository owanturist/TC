module Main exposing (main)

import Browser
import Chart exposing (Chart)
import DOM
import Data
import Dict exposing (Dict)
import Foo
import Html exposing (Html, div, text)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Lazy as Lazy
import Json.Decode as Decode exposing (Decoder, Value)
import Svg exposing (Svg, path, svg)
import Svg.Attributes
import Task
import Time
import Utils.DOM



-- M O D E L


type Dragging
    = NoDragging
    | SelectorFromChanging Selector Float Float
    | SelectorToChanging Selector Float Float
    | SelectorAreaChanging Selector Float Float


applyDragging : Dragging -> Float -> Maybe Selector
applyDragging dragging end =
    case dragging of
        NoDragging ->
            Nothing

        SelectorFromChanging { from, area } start width ->
            let
                -- keep minumun 48px (converted to pct) width for dragging
                delta =
                    clamp -from (area - 48 / width) ((end - start) / width)
            in
            Just
                { from = from + delta
                , area = area - delta
                }

        SelectorToChanging { from, area } start width ->
            let
                -- keep minumun 48px (converted to pct) width for dragging
                delta =
                    clamp (-area + 48 / width) (1 - from - area) ((end - start) / width)
            in
            Just
                { from = from
                , area = area + delta
                }

        SelectorAreaChanging { from, area } start width ->
            let
                delta =
                    clamp -from (1 - from - area) ((end - start) / width)
            in
            Just
                { from = from + delta
                , area = area
                }


type alias Selector =
    { from : Float
    , area : Float
    }


type alias Model =
    { selector : Selector
    , dragging : Dragging
    , chart : Result Decode.Error Data.Chart
    , foo : Foo.Model
    }


init : Value -> ( Model, Cmd Msg )
init json =
    let
        chart =
            Data.decode json

        initialFoo =
            case chart of
                Err _ ->
                    Foo.init
                        identity
                        identity
                        1000
                        ( 460, 460 )
                        []
                        Dict.empty

                Ok { axisX, lines } ->
                    Foo.init
                        (toFloat << Time.posixToMillis)
                        toFloat
                        500
                        ( 460, 460 )
                        axisX
                        lines
    in
    ( { selector = Selector 0 1
      , dragging = NoDragging
      , chart = Data.decode json
      , foo = initialFoo
      }
    , Cmd.none
    )



-- U P D A T E


type Msg
    = StartSelectorFromChanging Float Float
    | StartSelectorToChanging Float Float
    | StartSelectorAreaChanging Float Float
    | DragSelector Float
    | DragEndSelector Float
    | FooMsg Foo.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartSelectorFromChanging start width ->
            ( { model | dragging = SelectorFromChanging model.selector start width }
            , Cmd.none
            )

        StartSelectorToChanging start width ->
            ( { model | dragging = SelectorToChanging model.selector start width }
            , Cmd.none
            )

        StartSelectorAreaChanging start width ->
            ( { model | dragging = SelectorAreaChanging model.selector start width }
            , Cmd.none
            )

        DragSelector end ->
            ( { model | selector = Maybe.withDefault model.selector (applyDragging model.dragging end) }
            , Cmd.none
            )

        DragEndSelector end ->
            ( { model
                | dragging = NoDragging
                , selector = Maybe.withDefault model.selector (applyDragging model.dragging end)
              }
            , Cmd.none
            )

        FooMsg msgOfFoo ->
            case Foo.update msgOfFoo model.foo of
                Foo.Idle ->
                    ( model, Cmd.none )

                Foo.Updated nextFoo ->
                    ( { model | foo = nextFoo }
                    , Cmd.none
                    )



-- S U B S C R I P T I O N


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map FooMsg (Foo.subscriptions model.foo)



-- V I E W


stop : Decoder msg -> Decoder ( msg, Bool )
stop decoder =
    Decode.map (\msg -> ( msg, True )) decoder


withTouchX : (Float -> Msg) -> Decoder Msg
withTouchX tagger =
    Decode.float
        |> Decode.at [ "changedTouches", "0", "pageX" ]
        |> Decode.map tagger


withTouchXandSelectorWidth : (Float -> Float -> Msg) -> Decoder Msg
withTouchXandSelectorWidth tagger =
    Decode.map2 tagger
        (Decode.field "pageX" Decode.float)
        (Decode.float
            |> Decode.field "clientWidth"
            |> Utils.DOM.closest "main__overview-selector"
            |> DOM.target
        )
        |> Decode.at [ "changedTouches", "0" ]


pct : Float -> String
pct value =
    String.fromFloat value ++ "%"


makeViewBox : Int -> Int -> String
makeViewBox width height =
    String.join " "
        [ "0"
        , String.fromFloat (1.1 * toFloat -height)
        , String.fromInt width
        , String.fromFloat (1.1 * toFloat height)
        ]


viewPaths :
    { width : Int
    , height : Int
    , strokeWidth : Int
    }
    -> Chart
    -> List (Svg msg)
viewPaths { width, height, strokeWidth } chart =
    List.map
        (\line ->
            path
                [ Svg.Attributes.stroke line.color
                , Svg.Attributes.strokeWidth (String.fromInt strokeWidth)
                , Svg.Attributes.fill "none"
                , Svg.Attributes.d line.value
                ]
                []
        )
        (Chart.draw width height chart)


viewChart : Chart -> Html msg
viewChart chart =
    div []
        [ svg
            [ Svg.Attributes.class "main__svg"
            , Svg.Attributes.viewBox (makeViewBox 460 460)
            ]
            (viewPaths
                { width = 460
                , height = 460
                , strokeWidth = 3
                }
                chart
            )
        ]


viewOverviewSvg : Chart -> Svg msg
viewOverviewSvg chart =
    svg
        [ Svg.Attributes.class "main__svg"
        , Svg.Attributes.viewBox (makeViewBox 460 60)
        ]
        (viewPaths
            { width = 460
            , height = 60
            , strokeWidth = 1
            }
            chart
        )


viewOverviewSelector : Selector -> Dragging -> Html Msg
viewOverviewSelector selector dragging =
    let
        handlers =
            case dragging of
                NoDragging ->
                    []

                _ ->
                    [ Events.on "touchmove" (withTouchX DragSelector)
                    , Events.on "touchend" (withTouchX DragEndSelector)
                    ]
    in
    div
        (Attributes.class "main__overview-selector"
            :: handlers
        )
        [ div
            [ Attributes.class "main__overview-field"
            , Attributes.style "width" (pct (100 * selector.from))
            ]
            []
        , div
            [ Attributes.class "main__overview-field main__overview-field_active"
            , Attributes.style "width" (pct (100 * selector.area))
            , Events.on "touchstart" (withTouchXandSelectorWidth StartSelectorAreaChanging)
            ]
            [ div
                [ Attributes.class "main__overview-expander"
                , Events.stopPropagationOn "touchstart" (stop (withTouchXandSelectorWidth StartSelectorFromChanging))
                ]
                []
            , div
                [ Attributes.class "main__overview-expander main__overview-expander_end"
                , Events.stopPropagationOn "touchstart" (stop (withTouchXandSelectorWidth StartSelectorToChanging))
                ]
                []
            ]
        , div
            [ Attributes.class "main__overview-field main__overview-field_end"
            ]
            []
        ]


viewContainer : List (Html msg) -> Html msg
viewContainer children =
    div [ Attributes.class "main__container" ] children


view : Selector -> Dragging -> Chart -> Html Msg
view selector dragging chart =
    div
        [ Attributes.class "main"
        ]
        [ Lazy.lazy viewChart (Chart.select selector.from (selector.from + selector.area) chart)
        , viewContainer
            [ div
                [ Attributes.class "main__overview"
                ]
                [ Lazy.lazy viewOverviewSvg chart
                , Lazy.lazy2 viewOverviewSelector selector dragging
                ]
            ]
        ]



-- M A I N


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view =
            \model ->
                Browser.Document "Charts"
                    [ {--case model.chart of
                        Err err ->
                            text (Decode.errorToString err)

                        Ok chart ->
                            view
                                model.selector
                                model.dragging
                                (Chart.init (toFloat << Time.posixToMillis) toFloat chart.axisX chart.lines)
                    , --}Html.map FooMsg (Foo.view model.foo)
                    ]
        }
