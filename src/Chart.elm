module Chart exposing (Model, Msg, Settings, init, subscriptions, update, view)

import Browser.Events
import DOM
import Data
import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes
import Html.Events
import Html.Lazy
import Json.Decode as Decode exposing (Decoder)
import Regex
import Svg exposing (path, svg)
import Svg.Attributes
import Svg.Keyed



-- M O D E L


type alias Limits =
    { min : Float
    , max : Float
    }


type alias Timeline =
    List Float


type alias Lines =
    Dict String (Data.Line (List Float))


type alias Chart =
    Data.Chart Float Float


type alias Animation =
    { duration : Float
    }


type alias Settings =
    { animation : Animation
    }


type alias Range =
    { from : Float
    , to : Float
    }


type Dragging
    = NoDragging
    | SelectorFromChanging Range Float Float
    | SelectorToChanging Range Float Float
    | SelectorAreaChanging Range Float Float


applyDragging : Dragging -> Float -> Maybe Range
applyDragging dragging end =
    case dragging of
        NoDragging ->
            Nothing

        SelectorFromChanging { from, to } start width ->
            let
                -- keep minumun 48px (converted to pct) width for dragging
                delta =
                    clamp -from ((to - from) - 48 / width) ((end - start) / width)
            in
            Just
                { from = from + delta
                , to = to
                }

        SelectorToChanging { from, to } start width ->
            let
                -- keep minumun 48px (converted to pct) width for dragging
                delta =
                    clamp ((from - to) + 48 / width) (1 - to) ((end - start) / width)
            in
            Just
                { from = from
                , to = to + delta
                }

        SelectorAreaChanging { from, to } start width ->
            let
                delta =
                    clamp -from (1 - to) ((end - start) / width)
            in
            Just
                { from = from + delta
                , to = to + delta
                }


type Canvas
    = Empty
    | Static Limits Limits Timeline Lines
    | Animated Float Limits Limits Limits Timeline Lines


type alias State =
    { range : Range
    , dragging : Dragging
    , canvas : Canvas
    }


type Model
    = Model Settings Chart State


init : Settings -> Chart -> Model
init settings chart =
    let
        initialRange =
            Range 0 1
    in
    Model settings chart (State initialRange NoDragging (select initialRange settings chart Empty))


consToTimeline : Float -> ( Maybe Limits, Timeline ) -> ( Maybe Limits, Timeline )
consToTimeline x ( limits, acc ) =
    ( case limits of
        Nothing ->
            Just (Limits x x)

        Just prev ->
            Just (Limits prev.min x)
    , x :: acc
    )


consToLines : List ( String, Float ) -> ( Maybe Limits, Dict String (List Float) ) -> ( Maybe Limits, Dict String (List Float) )
consToLines bunch acc =
    List.foldr
        (\( key, y ) ( limits, lines ) ->
            ( case limits of
                Nothing ->
                    Just (Limits y y)

                Just prev ->
                    Just (Limits (min y prev.min) (max y prev.max))
            , Dict.update key
                (\result ->
                    case result of
                        Nothing ->
                            Just [ y ]

                        Just values ->
                            Just (y :: values)
                )
                lines
            )
        )
        acc
        bunch


approximate : (value -> value -> value) -> List value -> List ( key, value ) -> List ( key, value )
approximate approximator =
    List.map2 (\target ( key, value ) -> ( key, approximator value target ))


type Approximation a
    = NoApproximate
    | ToLeft a
    | ToRight a


type alias Selection =
    { timeline : ( Maybe Limits, Timeline )
    , values : ( Maybe Limits, Dict String (List Float) )
    , approximation : Approximation ( Float, Float, List Float )
    }


selectStep : Range -> Float -> Float -> Float -> List ( String, Float ) -> Selection -> Selection
selectStep { from, to } firstX lastX x bunch acc =
    let
        position =
            (x - firstX) / (lastX - firstX)

        approximatorLeft prevPosition current prev =
            prev + (current - prev) * (from - prevPosition) / (position - prevPosition)

        approximatorRight prevPosition current prev =
            prev + (current - prev) * (to - prevPosition) / (position - prevPosition)
    in
    if from <= position then
        if position <= to then
            case acc.approximation of
                ToLeft ( prevPosition, prevX, prevValues ) ->
                    let
                        approximatedX =
                            approximatorLeft prevPosition x prevX

                        approximatedValues =
                            approximate (approximatorLeft prevPosition) prevValues bunch
                    in
                    { timeline = consToTimeline x (consToTimeline approximatedX acc.timeline)
                    , values = consToLines bunch (consToLines approximatedValues acc.values)
                    , approximation = ToRight ( position, x, List.map Tuple.second bunch )
                    }

                _ ->
                    { timeline = consToTimeline x acc.timeline
                    , values = consToLines bunch acc.values
                    , approximation = ToRight ( position, x, List.map Tuple.second bunch )
                    }

        else
            case acc.approximation of
                NoApproximate ->
                    acc

                ToLeft ( prevPosition, prevX, prevValues ) ->
                    let
                        aproximatedLeftX =
                            approximatorLeft prevPosition x prevX

                        aproximatedRightX =
                            approximatorRight prevPosition x prevX

                        approximatedLeftValues =
                            approximate (approximatorLeft prevPosition) prevValues bunch

                        approximatedRightValues =
                            approximate (approximatorRight prevPosition) prevValues bunch
                    in
                    { timeline = List.foldr consToTimeline acc.timeline [ aproximatedRightX, aproximatedLeftX ]
                    , values = List.foldr consToLines acc.values [ approximatedRightValues, approximatedLeftValues ]
                    , approximation = NoApproximate
                    }

                ToRight ( prevPosition, prevX, prevValues ) ->
                    let
                        approximatedX =
                            approximatorRight prevPosition x prevX

                        approximatedValues =
                            approximate (approximatorRight prevPosition) prevValues bunch
                    in
                    { timeline = consToTimeline approximatedX acc.timeline
                    , values = consToLines approximatedValues acc.values
                    , approximation = NoApproximate
                    }

    else
        { acc | approximation = ToLeft ( position, x, List.map Tuple.second bunch ) }


select : Range -> Settings -> Chart -> Canvas -> Canvas
select range { animation } chart canvas =
    let
        ( firstX, lastX ) =
            ( Data.firstChartX chart, Data.lastChartX chart )

        selection =
            -- it builds timeline and Line.value in reversed order, should be reversed again later
            Data.foldlChart
                (selectStep range firstX lastX)
                { timeline = ( Nothing, [] )
                , values = ( Nothing, Dict.empty )
                , approximation = NoApproximate
                }
                chart
    in
    case
        ( Dict.merge
            {- indicate a difference between input and output lineIds dicts -} (\_ _ _ -> Nothing)
            (\lineId nextValue line -> Maybe.map (Dict.insert lineId (Data.setLineValue nextValue line)))
            {- indicate a difference between input and output lineIds dicts -} (\_ _ _ -> Nothing)
            (Tuple.second selection.values)
            chart.lines
            (Just Dict.empty)
        , Tuple.first selection.timeline
        , Maybe.map (\limits -> Limits (min 0 limits.min) (max 0 limits.max)) (Tuple.first selection.values)
        )
    of
        ( Just values, Just limitsX, Just limitsY ) ->
            case canvas of
                Empty ->
                    Static limitsX limitsY (Tuple.second selection.timeline) values

                Static _ prevLimitsY _ _ ->
                    if prevLimitsY == limitsY then
                        Static limitsX limitsY (Tuple.second selection.timeline) values

                    else
                        Animated animation.duration
                            limitsX
                            prevLimitsY
                            limitsY
                            (Tuple.second selection.timeline)
                            values

                Animated countdown _ limitsYStart limitsYEnd _ _ ->
                    if limitsYEnd == limitsY then
                        Animated countdown
                            limitsX
                            limitsYStart
                            limitsYEnd
                            (Tuple.second selection.timeline)
                            values

                    else
                        let
                            done =
                                easeOutQuad (1 - countdown / animation.duration)
                        in
                        Animated animation.duration
                            limitsX
                            { min = calcDoneLimit limitsYStart.min limitsYEnd.min done
                            , max = calcDoneLimit limitsYStart.max limitsYEnd.max done
                            }
                            limitsY
                            (Tuple.second selection.timeline)
                            values

        _ ->
            Empty



-- U P D A T E


type Msg
    = StartSelectorFromChanging Float Float
    | StartSelectorToChanging Float Float
    | StartSelectorAreaChanging Float Float
    | DragSelector Float
    | DragEndSelector Float
    | Tick Float


update : Msg -> Model -> Model
update msg (Model settings chart state) =
    case msg of
        StartSelectorFromChanging start width ->
            Model settings
                chart
                { state | dragging = SelectorFromChanging state.range start width }

        StartSelectorToChanging start width ->
            Model settings
                chart
                { state | dragging = SelectorToChanging state.range start width }

        StartSelectorAreaChanging start width ->
            Model settings
                chart
                { state | dragging = SelectorAreaChanging state.range start width }

        DragSelector end ->
            case applyDragging state.dragging end of
                Nothing ->
                    Model settings chart state

                Just nextRange ->
                    Model settings
                        chart
                        { state
                            | range = nextRange
                            , canvas = select nextRange settings chart state.canvas
                        }

        DragEndSelector _ ->
            Model settings
                chart
                { state | dragging = NoDragging }

        Tick delta ->
            case state.canvas of
                Animated countdown limitsX limitsYStart limitsYEnd timeline lines ->
                    let
                        nextCanvas =
                            if delta >= countdown then
                                Static limitsX limitsYEnd timeline lines

                            else
                                Animated (countdown - delta) limitsX limitsYStart limitsYEnd timeline lines
                    in
                    Model settings chart { state | canvas = nextCanvas }

                _ ->
                    Model settings chart state



-- S U B S C R I P T I O N


subscriptions : Model -> Sub Msg
subscriptions (Model _ _ state) =
    case state.canvas of
        Animated _ _ _ _ _ _ ->
            Browser.Events.onAnimationFrameDelta Tick

        _ ->
            Sub.none



-- V I E W


type alias ViewBox =
    { width : Int
    , height : Int
    }


makeViewBox : ViewBox -> String
makeViewBox { width, height } =
    [ 0, 0, width, height ]
        |> List.map String.fromInt
        |> String.join " "


type alias Config =
    { viewBox : ViewBox
    }


config : Config
config =
    Config (ViewBox 460 460)


containsClass : String -> String -> Bool
containsClass x className =
    case
        Regex.fromStringWith
            { caseInsensitive = False
            , multiline = False
            }
            ("(^|\\s+)" ++ x ++ "($|\\s+)")
    of
        Nothing ->
            False

        Just regex ->
            Regex.contains regex className


closest : String -> Decoder node -> Decoder node
closest class decoder =
    Decode.andThen
        (\className ->
            if containsClass class className then
                decoder

            else
                DOM.parentElement (closest class decoder)
        )
        DOM.className


stop : Decoder msg -> Decoder ( msg, Bool )
stop decoder =
    Decode.map (\msg -> ( msg, True )) decoder


withTouchX : (Float -> msg) -> Decoder msg
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
            |> closest "main__overview-selector"
            |> DOM.target
        )
        |> Decode.at [ "changedTouches", "0" ]


pct : Float -> String
pct value =
    String.fromFloat value ++ "%"


coordinate : Float -> Float -> String
coordinate x y =
    String.fromFloat x ++ "," ++ String.fromFloat y


drawStep : (Float -> Float) -> (Float -> Float) -> Timeline -> List Float -> String
drawStep mapX mapY timeline points =
    case ( timeline, points ) of
        ( firstX :: restX, firstY :: restY ) ->
            List.foldl
                (\x ( axisY, acc ) ->
                    case axisY of
                        [] ->
                            ( [], acc )

                        y :: nextY ->
                            ( nextY
                            , acc ++ "L" ++ coordinate (mapX x) (mapY y)
                            )
                )
                ( restY, "M" ++ coordinate (mapX firstX) (mapY firstY) )
                restX
                |> Tuple.second

        _ ->
            ""


drawHelp : (Float -> Float) -> (Float -> Float) -> Timeline -> Lines -> List (Data.Line String)
drawHelp mapX mapY timeline lines =
    List.map
        (Data.mapLineValue (drawStep mapX mapY timeline))
        (Dict.values lines)


easeOutQuad : Float -> Float
easeOutQuad delta =
    delta * (2 - delta)


calcScale : Int -> Float -> Float -> Float
calcScale side min max =
    if min == max then
        0

    else
        toFloat side / (max - min)


calcDoneLimit : Float -> Float -> Float -> Float
calcDoneLimit start end done =
    start + (end - start) * done


draw : Settings -> Canvas -> List (Data.Line String)
draw { animation } canvas =
    case canvas of
        Empty ->
            []

        Static limitsX limitsY timeline lines ->
            let
                scaleX =
                    calcScale config.viewBox.width limitsX.min limitsX.max

                scaleY =
                    calcScale config.viewBox.height limitsY.min limitsY.max
            in
            drawHelp
                (\x -> scaleX * (x - limitsX.min))
                (\y -> scaleY * (limitsY.max - y))
                timeline
                lines

        Animated countdown limitsX limitsYStart limitsYEnd timeline lines ->
            let
                scaleX =
                    calcScale config.viewBox.width limitsX.min limitsX.max

                done =
                    easeOutQuad (1 - countdown / animation.duration)

                limitYMin =
                    calcDoneLimit limitsYStart.min limitsYEnd.min done

                limitYMax =
                    calcDoneLimit limitsYStart.max limitsYEnd.max done

                scaleY =
                    calcScale config.viewBox.height limitYMin limitYMax
            in
            drawHelp
                (\x -> scaleX * (x - limitsX.min))
                (\y -> scaleY * (limitYMax - y))
                timeline
                lines


viewOverviewSelector : Range -> Dragging -> Html Msg
viewOverviewSelector range dragging =
    let
        handlers =
            case dragging of
                NoDragging ->
                    []

                _ ->
                    [ Html.Events.on "touchmove" (withTouchX DragSelector)
                    , Html.Events.on "touchend" (withTouchX DragEndSelector)
                    ]
    in
    div
        (Html.Attributes.class "main__overview-selector"
            :: handlers
        )
        [ div
            [ Html.Attributes.class "main__overview-field"
            , Html.Attributes.style "width" (pct (100 * range.from))
            ]
            []
        , div
            [ Html.Attributes.class "main__overview-field main__overview-field_active"
            , Html.Attributes.style "width" (pct (100 * (range.to - range.from)))
            , Html.Events.on "touchstart" (withTouchXandSelectorWidth StartSelectorAreaChanging)
            ]
            [ div
                [ Html.Attributes.class "main__overview-expander"
                , Html.Events.stopPropagationOn "touchstart" (stop (withTouchXandSelectorWidth StartSelectorFromChanging))
                ]
                []
            , div
                [ Html.Attributes.class "main__overview-expander main__overview-expander_end"
                , Html.Events.stopPropagationOn "touchstart" (stop (withTouchXandSelectorWidth StartSelectorToChanging))
                ]
                []
            ]
        , div
            [ Html.Attributes.class "main__overview-field main__overview-field_end"
            ]
            []
        ]


view : Model -> Html Msg
view (Model settings chart state) =
    div
        []
        [ svg
            [ Svg.Attributes.viewBox (makeViewBox config.viewBox)
            ]
            [ Svg.Keyed.node "g"
                []
                (List.map
                    (\line ->
                        ( line.id
                        , path
                            [ Svg.Attributes.stroke line.color
                            , Svg.Attributes.strokeWidth "1.5"
                            , Svg.Attributes.fill "none"
                            , Svg.Attributes.d line.value
                            ]
                            []
                        )
                    )
                    (draw settings state.canvas)
                )
            ]
        , Html.Lazy.lazy2 viewOverviewSelector state.range state.dragging
        ]
