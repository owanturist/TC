module Main exposing (main)

import Browser
import DOM
import Data
import Html exposing (Html, div, text)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
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
    , charts : List (Data.Chart Int)
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { selector = Selector 0.2 0.4
      , dragging = NoDragging
      , charts = []
      }
    , Task.perform (GenerateData << Data.generate 60) Time.now
    )



-- U P D A T E


type Msg
    = GenerateData (List (Data.Chart Int))
    | StartSelectorFromChanging Float Float
    | StartSelectorToChanging Float Float
    | StartSelectorAreaChanging Float Float
    | DragSelector Float
    | DragEndSelector Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateData generatedCharts ->
            ( { model | charts = generatedCharts }
            , Cmd.none
            )

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


viewChart : Float -> Float -> Data.Chart Int -> Maybe (Svg msg)
viewChart scaleX scaleY chart =
    chart.data
        |> List.indexedMap (\i ( _, value ) -> ( scaleX * toFloat i, scaleY * toFloat value ))
        |> calculatePath
        |> Maybe.map
            (\dValue ->
                path
                    [ Svg.Attributes.stroke chart.color
                    , Svg.Attributes.strokeWidth "1"
                    , Svg.Attributes.fill "none"
                    , Svg.Attributes.d dValue
                    ]
                    []
            )


viewCharts : Int -> Int -> List (Data.Chart Int) -> Svg msg
viewCharts width height charts =
    -- TODO optimize maximum and size
    case
        List.foldr
            (\chart acc ->
                case ( acc, List.maximum (List.map Tuple.second chart.data) ) of
                    ( Nothing, Nothing ) ->
                        Nothing

                    ( Nothing, Just maximum ) ->
                        Just
                            { maximum = maximum
                            , size = List.length chart.data
                            }

                    ( prev, Nothing ) ->
                        prev

                    ( Just prev, Just maximum ) ->
                        Just
                            { maximum = max prev.maximum maximum
                            , size = min prev.size (List.length chart.data)
                            }
            )
            Nothing
            charts
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
                    (viewChart (toFloat width / toFloat (size - 1)) (toFloat height / toFloat maximum))
                    charts
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
            [ Attributes.class "main__overview-field main__overview-field_end" ]
            []
        ]


viewContainer : List (Html msg) -> Html msg
viewContainer children =
    div [ Attributes.class "main__container" ] children


view : Model -> Html Msg
view model =
    div
        [ Attributes.class "main"
        ]
        [ viewContainer
            [ div
                [ Attributes.class "main__overview"
                ]
                [ viewCharts 460 60 model.charts
                , viewOverviewSelector model.selector model.dragging
                ]
            ]
        ]



-- M A I N


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view =
            \model ->
                Browser.Document "Charts"
                    [ view model
                    ]
        }
