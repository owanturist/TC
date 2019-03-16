module Foo exposing (Config, Model, Msg, Settings, init, select, subscriptions, update, view)

import Browser.Events
import Data
import Dict exposing (Dict)
import Svg exposing (Svg, path, svg)
import Svg.Attributes
import Svg.Keyed


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


type State
    = Empty
    | Static Limits Limits Timeline Lines
    | Animated Float Limits Limits Limits Timeline Lines


type Model
    = Model Settings Chart State


init : ( Float, Float ) -> Settings -> Chart -> Model
init selector settings chart =
    Model
        settings
        chart
        (selectHelp selector settings chart Empty)


select : ( Float, Float ) -> Model -> Model
select range (Model settings chart state) =
    Model settings chart (selectHelp range settings chart state)


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
    = NotApproximate
    | ToLeft a
    | ToRight a


type alias Selection =
    { timeline : ( Maybe Limits, Timeline )
    , values : ( Maybe Limits, Dict String (List Float) )
    , approximation : Approximation ( Float, Float, List Float )
    }


selectStep : Float -> Float -> Float -> Float -> Float -> List ( String, Float ) -> Selection -> Selection
selectStep from to firstX lastX x bunch acc =
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
                NotApproximate ->
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
                    , approximation = NotApproximate
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
                    , approximation = NotApproximate
                    }

    else
        { acc | approximation = ToLeft ( position, x, List.map Tuple.second bunch ) }


selectHelp : ( Float, Float ) -> Settings -> Chart -> State -> State
selectHelp ( from, to ) { animation } chart state =
    let
        to_ =
            clamp 0 1 to

        from_ =
            clamp 0 to_ from

        ( firstX, lastX ) =
            ( Data.firstChartX chart, Data.lastChartX chart )

        selection =
            -- it builds timeline and Line.value in reversed order, should be reversed again later
            Data.foldlChart
                (selectStep from to firstX lastX)
                { timeline = ( Nothing, [] )
                , values = ( Nothing, Dict.empty )
                , approximation = NotApproximate
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
            case state of
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
    = Tick Float


update : Msg -> Model -> Model
update msg (Model settings chart state) =
    case msg of
        Tick delta ->
            case state of
                Animated countdown limitsX limitsYStart limitsYEnd timeline lines ->
                    if delta >= countdown then
                        Static limitsX limitsYEnd timeline lines
                            |> Model settings chart

                    else
                        Animated (countdown - delta) limitsX limitsYStart limitsYEnd timeline lines
                            |> Model settings chart

                _ ->
                    Model settings chart state



-- S U B S C R I P T I O N


subscriptions : Model -> Sub Msg
subscriptions (Model settings chart state) =
    case state of
        Animated _ _ _ _ _ _ ->
            Browser.Events.onAnimationFrameDelta Tick

        _ ->
            Sub.none



-- V I E W


type alias ViewBox =
    { width : Int
    , height : Int
    }


type alias Config =
    { viewBox : ViewBox
    }


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


calcDoneLimit : Float -> Float -> Float -> Float
calcDoneLimit start end done =
    start + (end - start) * done


draw : Config -> Settings -> State -> List (Data.Line String)
draw { viewBox } { animation } state =
    case state of
        Empty ->
            []

        Static limitsX limitsY timeline lines ->
            let
                scaleX =
                    if limitsX.min == limitsX.max then
                        0

                    else
                        toFloat viewBox.width / (limitsX.max - limitsX.min)

                scaleY =
                    if limitsY.min == limitsY.max then
                        0

                    else
                        toFloat viewBox.height / (limitsY.max - limitsY.min)
            in
            drawHelp
                (\x -> scaleX * (x - limitsX.min))
                (\y -> scaleY * (limitsY.max - y))
                timeline
                lines

        Animated countdown limitsX limitsYStart limitsYEnd timeline lines ->
            let
                scaleX =
                    if limitsX.min == limitsX.max then
                        0

                    else
                        toFloat viewBox.width / (limitsX.max - limitsX.min)

                done =
                    easeOutQuad (1 - countdown / animation.duration)

                limitYMin =
                    calcDoneLimit limitsYStart.min limitsYEnd.min done

                limitYMax =
                    calcDoneLimit limitsYStart.max limitsYEnd.max done

                scaleY =
                    if limitYMax == limitYMin then
                        0

                    else
                        toFloat viewBox.height / (limitYMax - limitYMin)
            in
            drawHelp
                (\x -> scaleX * (x - limitsX.min))
                (\y -> scaleY * (limitYMax - y))
                timeline
                lines


makeViewBox : ViewBox -> String
makeViewBox { width, height } =
    [ 0, 0, width, height ]
        |> List.map String.fromInt
        |> String.join " "


view : Config -> Model -> Svg msg
view config (Model settings chart state) =
    svg
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
                (draw config settings state)
            )
        ]
