module Main exposing (main)

import Browser
import Chart exposing (Chart)
import DOM
import Dict exposing (Dict)
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
    , chart : Result Decode.Error (Chart Int Float)
    }


init : Value -> ( Model, Cmd Msg )
init json =
    ( { selector = Selector 0 1
      , dragging = NoDragging
      , chart = Result.map (Chart.mapXY Time.posixToMillis toFloat) (Chart.decode json)
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



{--
viewLine : Float -> Float -> Int -> Data.Line Float -> Maybe (Svg msg)
viewLine scaleX scaleY strokeWidth chart =
    chart.points
        |> List.indexedMap (\i value -> ( scaleX * toFloat i, scaleY * -value ))
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
--}


makeViewBox : Int -> Int -> String
makeViewBox width height =
    String.join " "
        [ "0"
        , String.fromFloat (1.1 * toFloat -height)
        , String.fromInt width
        , String.fromFloat (1.1 * toFloat height)
        ]


makePaths : Float -> Float -> Int -> Chart Int Float -> Dict String String
makePaths scaleX scaleY shiftX =
    Chart.foldl
        (\x points paths ->
            List.foldr
                (\( id, y ) ->
                    Dict.update id
                        (\path ->
                            case path of
                                Nothing ->
                                    Just (m True ( scaleX * toFloat (x - shiftX), scaleY * -y ))

                                Just prev ->
                                    Just (prev ++ l True ( scaleX * toFloat (x - shiftX), scaleY * -y ))
                        )
                )
                paths
                points
        )
        Dict.empty


viewChart :
    { width : Int
    , height : Int
    , strokeWidth : Int
    }
    -> Chart Int Float
    -> Svg msg
viewChart { width, height, strokeWidth } chart =
    let
        limits =
            { minX = Chart.minimumX chart
            , maxX = Chart.maximumX chart
            , minY = Maybe.map (min 0) (Chart.minimumY chart)
            , maxY = Chart.maximumY chart
            }

        scaleX =
            case Maybe.map2 (-) limits.maxX limits.minX of
                Nothing ->
                    1

                Just deltaX ->
                    toFloat width / toFloat deltaX

        scaleY =
            case Maybe.map2 (-) limits.maxY limits.minY of
                Nothing ->
                    1

                Just deltaY ->
                    toFloat height / deltaY

        shiftX =
            Maybe.withDefault 0 limits.minX

        paths =
            makePaths scaleX scaleY shiftX chart
                |> Dict.toList
                |> List.filterMap
                    (\( id, path ) ->
                        Maybe.map2
                            (\name color ->
                                { id = id
                                , name = name
                                , color = color
                                , path = path
                                }
                            )
                            (Chart.getName id chart)
                            (Chart.getColor id chart)
                    )
    in
    svg
        [ Svg.Attributes.class "main__svg"
        , Svg.Attributes.viewBox (makeViewBox width height)
        ]
        {--(case
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
                []

            Just { maximum, size } ->
                List.filterMap
                    (viewLine (toFloat width / toFloat (size - 1)) (toFloat height / maximum) strokeWidth)
                    chart.lines
        )
        --}
        (List.map
            (\config ->
                path
                    [ Svg.Attributes.stroke config.color
                    , Svg.Attributes.strokeWidth (String.fromInt strokeWidth)
                    , Svg.Attributes.fill "none"
                    , Svg.Attributes.d config.path
                    ]
                    []
            )
            paths
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


foo : Selector -> List Float -> List Float
foo selector list =
    let
        from =
            selector.from

        to =
            selector.from + selector.area

        lastIndex =
            toFloat (List.length list - 1)
    in
    List.foldr
        (\el { index, result, left, right } ->
            let
                boundary =
                    1 - index / lastIndex

                ( nextResult, nextLeft, nextRight ) =
                    if from <= boundary then
                        if boundary <= to then
                            ( case right of
                                Nothing ->
                                    el :: result

                                Just ( b, r ) ->
                                    el :: el + ((r - el) * (to - boundary) / (b - boundary)) :: result
                            , Just ( boundary, el )
                            , Nothing
                            )

                        else
                            ( result
                            , left
                            , Just ( boundary, el )
                            )

                    else
                        case left of
                            Nothing ->
                                if List.isEmpty result then
                                    case right of
                                        Nothing ->
                                            ( result, left, right )

                                        Just ( b, r ) ->
                                            ( (el + ((r - el) * (from - boundary) / (b - boundary)))
                                                :: (el + ((r - el) * (to - boundary) / (b - boundary)))
                                                :: result
                                            , left
                                            , right
                                            )

                                else
                                    ( result, left, right )

                            Just ( b, le ) ->
                                ( le + ((el - le) * (b - from) / (b - boundary)) :: result
                                , Nothing
                                , right
                                )
            in
            { index = index + 1
            , result = nextResult
            , left = nextLeft
            , right = nextRight
            }
        )
        { index = 0
        , result = []
        , left = Nothing
        , right = Nothing
        }
        list
        |> .result


view : Selector -> Dragging -> Chart Int Float -> Html Msg
view selector dragging chart =
    div
        [ Attributes.class "main"
        ]
        [ {--viewChart
            { width = 460
            , height = 460
            , strokeWidth = 3
            }
            { chart | lines = bar }
        ,--}
          viewContainer
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

        --}
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
