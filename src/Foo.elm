module Foo exposing (Config, Model, Msg, Settings, init, select, subscriptions, update, view)

import Browser.Events
import Data
import Dict exposing (Dict)
import Svg exposing (Svg, path, svg)
import Svg.Attributes


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


type alias Foo =
    { selectedTimeline : ( Maybe Limits, Timeline )
    , selectedValues : ( Maybe Limits, Dict String (List Float) )
    , approximation : Approximation ( Float, Float, List Float )
    }


selectStep : Float -> Float -> Float -> Float -> List ( String, Float ) -> Foo -> Foo
selectStep from to position x values acc =
    let
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
                            approximate (approximatorLeft prevPosition) prevValues values
                    in
                    { selectedTimeline = consToTimeline x (consToTimeline approximatedX acc.selectedTimeline)
                    , selectedValues = consToLines values (consToLines approximatedValues acc.selectedValues)
                    , approximation = ToRight ( position, x, List.map Tuple.second values )
                    }

                _ ->
                    { selectedTimeline = consToTimeline x acc.selectedTimeline
                    , selectedValues = consToLines values acc.selectedValues
                    , approximation = ToRight ( position, x, List.map Tuple.second values )
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
                            approximate (approximatorLeft prevPosition) prevValues values

                        approximatedRightValues =
                            approximate (approximatorRight prevPosition) prevValues values
                    in
                    { selectedTimeline = List.foldr consToTimeline acc.selectedTimeline [ aproximatedRightX, aproximatedLeftX ]
                    , selectedValues = List.foldr consToLines acc.selectedValues [ approximatedRightValues, approximatedLeftValues ]
                    , approximation = NotApproximate
                    }

                ToRight ( prevPosition, prevX, prevValues ) ->
                    let
                        approximatedX =
                            approximatorRight prevPosition x prevX

                        approximatedValues =
                            approximate (approximatorRight prevPosition) prevValues values
                    in
                    { selectedTimeline = consToTimeline approximatedX acc.selectedTimeline
                    , selectedValues = consToLines approximatedValues acc.selectedValues
                    , approximation = NotApproximate
                    }

    else
        { acc | approximation = ToLeft ( position, x, List.map Tuple.second values ) }


selectHelp : ( Float, Float ) -> Settings -> Chart -> State -> State
selectHelp ( from, to ) { animation } data state =
    let
        to_ =
            clamp 0 1 to

        from_ =
            clamp 0 to_ from

        lastIndex =
            toFloat (data.size - 1)

        ( _, { selectedTimeline, selectedValues } ) =
            -- it builds timeline and Line.value in reversed order, should be reversed again later
            Data.foldlChart
                (\x values ( index, acc ) ->
                    ( index + 1
                    , selectStep from to (index / lastIndex) x values acc
                    )
                )
                ( 0
                , { selectedTimeline = ( Nothing, [] )
                  , selectedValues = ( Nothing, Dict.empty )
                  , approximation = NotApproximate
                  }
                )
                data
    in
    case
        ( Dict.merge
            {- indicate a difference between input and output lineIds dicts -} (\_ _ _ -> Nothing)
            (\lineId nextValue line -> Maybe.map (Dict.insert lineId (Data.setLineValue nextValue line)))
            {- indicate a difference between input and output lineIds dicts -} (\_ _ _ -> Nothing)
            (Tuple.second selectedValues)
            data.lines
            (Just Dict.empty)
        , Tuple.first selectedTimeline
          -- , Maybe.map (\limits -> Limits (min 0 limits.min) (max 0 limits.max)) (Tuple.first selectedValues)
        , Tuple.first selectedValues
        )
    of
        ( Just nextCorrectLines, Just limitsX, Just limitsY ) ->
            case state of
                Empty ->
                    Static limitsX limitsY (Tuple.second selectedTimeline) nextCorrectLines

                Static _ prevLimitsY _ _ ->
                    if prevLimitsY == limitsY then
                        Static limitsX limitsY (Tuple.second selectedTimeline) nextCorrectLines

                    else
                        Animated animation.duration
                            limitsX
                            prevLimitsY
                            limitsY
                            (Tuple.second selectedTimeline)
                            nextCorrectLines

                Animated countdown _ limitsYStart limitsYEnd _ _ ->
                    if limitsYEnd == limitsY then
                        Animated countdown
                            limitsX
                            limitsYStart
                            limitsYEnd
                            (Tuple.second selectedTimeline)
                            nextCorrectLines

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
                            (Tuple.second selectedTimeline)
                            nextCorrectLines

        _ ->
            Empty



-- U P D A T E


type Msg
    = Tick Float


update : Msg -> Model -> Model
update msg (Model settings data state) =
    case msg of
        Tick delta ->
            case state of
                Animated countdown limitsX limitsYStart limitsYEnd timeline lines ->
                    if delta >= countdown then
                        Static limitsX limitsYEnd timeline lines
                            |> Model settings data

                    else
                        Animated (countdown - delta) limitsX limitsYStart limitsYEnd timeline lines
                            |> Model settings data

                _ ->
                    Model settings data state



-- S U B S C R I P T I O N


subscriptions : Model -> Sub Msg
subscriptions (Model settings data state) =
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
view config (Model settings data state) =
    svg
        [ Svg.Attributes.viewBox (makeViewBox config.viewBox)
        ]
        (List.map
            (\line ->
                path
                    [ Svg.Attributes.stroke line.color
                    , Svg.Attributes.strokeWidth "1.5"
                    , Svg.Attributes.fill "none"
                    , Svg.Attributes.d line.value
                    ]
                    []
            )
            (draw config settings state)
        )
