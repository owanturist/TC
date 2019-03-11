module Main exposing (main)

import Browser
import DOM
import Data
import Html exposing (Html, div, text)
import Html.Attributes as Attributes
import Html.Events as Events
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
    }


init : Value -> ( Model, Cmd Msg )
init json =
    ( { selector = Selector 0.2 0.4
      , dragging = NoDragging
      , chart = Data.decode json
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


pathCoordinate : ( Float, Float ) -> String
pathCoordinate ( x, y ) =
    String.fromFloat x ++ "," ++ String.fromFloat y


m : Bool -> ( Float, Float ) -> String
m absolute coordinate =
    if absolute then
        "M" ++ pathCoordinate coordinate

    else
        "m" ++ pathCoordinate coordinate


l : Bool -> ( Float, Float ) -> String
l absolute coordinate =
    if absolute then
        "L" ++ pathCoordinate coordinate

    else
        "l" ++ pathCoordinate coordinate


calculatePath : List ( Float, Float ) -> Maybe String
calculatePath points =
    case points of
        first :: second :: rest ->
            m True first
                :: l True second
                :: List.map (l True) rest
                |> String.join " "
                |> Just

        _ ->
            Nothing


viewLine : Float -> Float -> Int -> Data.Line -> Maybe (Svg msg)
viewLine scaleX scaleY strokeWidth chart =
    chart.points
        |> List.indexedMap (\i value -> ( scaleX * toFloat i, scaleY * toFloat value ))
        |> calculatePath
        |> Maybe.map
            (\dValue ->
                path
                    [ Svg.Attributes.stroke chart.color
                    , Svg.Attributes.strokeWidth (String.fromInt strokeWidth)
                    , Svg.Attributes.fill "none"
                    , Svg.Attributes.d dValue
                    ]
                    []
            )


viewChart :
    { width : Int
    , height : Int
    , strokeWidth : Int
    }
    -> Data.Chart
    -> Svg msg
viewChart { width, height, strokeWidth } chart =
    -- TODO optimize maximum and size
    case
        List.foldr
            (\line acc ->
                case ( acc, List.maximum line.points ) of
                    ( Nothing, Nothing ) ->
                        Nothing

                    ( Nothing, Just maximum ) ->
                        Just
                            { maximum = maximum
                            , size = List.length line.points
                            }

                    ( prev, Nothing ) ->
                        prev

                    ( Just prev, Just maximum ) ->
                        Just
                            { maximum = max prev.maximum maximum
                            , size = min prev.size (List.length line.points)
                            }
            )
            Nothing
            chart.lines
    of
        Nothing ->
            svg
                [ Svg.Attributes.class "main__svg"
                ]
                []

        Just { maximum, size } ->
            svg
                [ Svg.Attributes.class "main__svg"
                , Svg.Attributes.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
                ]
                (List.filterMap
                    (viewLine (toFloat width / toFloat (size - 1)) (toFloat height / toFloat maximum) strokeWidth)
                    chart.lines
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


foo : Selector -> List a -> List a
foo selector list =
    List.foldr
        (\el { index, result, size } ->
            let
                current =
                    1 - toFloat index / toFloat size

                nextResult =
                    if selector.from <= current && current <= (selector.from + selector.area) then
                        el :: result

                    else
                        result
            in
            { index = index + 1
            , result = nextResult
            , size = size
            }
        )
        { index = 0
        , result = []
        , size = List.length list
        }
        list
        |> .result


view : Selector -> Dragging -> Data.Chart -> Html Msg
view selector dragging chart =
    let
        bar =
            List.map (\line -> { line | points = foo selector line.points }) chart.lines
    in
    div
        [ Attributes.class "main"
        ]
        [ viewChart
            { width = 460
            , height = 460
            , strokeWidth = 3
            }
            { chart | lines = bar }
        , viewContainer
            [ div
                [ Attributes.class "main__overview"
                ]
                [ viewChart
                    { width = 460
                    , height = 60
                    , strokeWidth = 1
                    }
                    chart
                , viewOverviewSelector selector dragging
                ]
            ]
        ]



-- M A I N


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view =
            \model ->
                Browser.Document "Charts"
                    [ case model.chart of
                        Err err ->
                            text (Decode.errorToString err)

                        Ok chart ->
                            view model.selector model.dragging chart
                    ]
        }
