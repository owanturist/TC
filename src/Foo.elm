module Foo exposing (Model, Msg, Stage(..), init, select, subscriptions, update, view)

import Browser.Events
import Dict exposing (Dict)
import Svg exposing (Svg, path, svg)
import Svg.Attributes


type alias Line a =
    { name : String
    , color : String
    , value : a
    }


map : (a -> b) -> Line a -> Line b
map fn line =
    set (fn line.value) line


set : a -> Line b -> Line a
set nextValue { name, color } =
    Line name color nextValue


type alias ViewBox =
    { width : Int
    , height : Int
    }


type alias Config =
    { duration : Float
    , viewBox : ViewBox
    }


type alias Limits =
    { min : Float
    , max : Float
    }


type alias Timeline =
    List Float


type alias Lines =
    Dict String (Line (List Float))


type alias Data =
    { size : Int
    , timeline : Timeline
    , lines : Lines
    }


foldNext : List ( key, List y ) -> Maybe ( List ( key, y ), List ( key, List y ) )
foldNext =
    List.foldr
        (\( key, value ) acc ->
            case acc of
                Nothing ->
                    Nothing

                Just ( heads, tails ) ->
                    case value of
                        first :: rest ->
                            Just
                                ( ( key, first ) :: heads
                                , ( key, rest ) :: tails
                                )

                        [] ->
                            Nothing
        )
        (Just ( [], [] ))


foldStep : (x -> List ( key, y ) -> acc -> acc) -> x -> ( acc, List ( key, List y ) ) -> ( acc, List ( key, List y ) )
foldStep fn x ( acc, values ) =
    case foldNext values of
        Nothing ->
            ( acc, values )

        Just ( heads, tails ) ->
            ( fn x heads acc, tails )


foldl : (Float -> List ( String, Float ) -> acc -> acc) -> acc -> Timeline -> Lines -> acc
foldl fn acc timeline lines =
    List.foldl
        (foldStep fn)
        ( acc, List.map (Tuple.mapSecond .value) (Dict.toList lines) )
        timeline
        |> Tuple.first


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
                            , acc ++ "L" ++ coordinate (mapX x) (mapY -y)
                            )
                )
                ( restY, "M" ++ coordinate (mapX firstX) (mapY -firstY) )
                restX
                |> Tuple.second

        _ ->
            ""


drawHelp : (Float -> Float) -> (Float -> Float) -> Timeline -> Lines -> List (Line String)
drawHelp mapX mapY timeline lines =
    List.map
        (map (drawStep mapX mapY timeline))
        (Dict.values lines)


easeOutQuad : Float -> Float
easeOutQuad delta =
    delta * (2 - delta)


draw : Config -> State -> List (Line String)
draw config state =
    case state of
        Empty ->
            []

        Static limitsX limitsY timeline lines ->
            let
                scaleX =
                    if limitsX.min == limitsX.max then
                        0

                    else
                        toFloat config.viewBox.width / (limitsX.max - limitsX.min)

                scaleY =
                    if limitsY.min == limitsY.max then
                        0

                    else
                        toFloat config.viewBox.height / (limitsY.max - limitsY.min)
            in
            drawHelp
                (\x -> scaleX * (x - limitsX.min))
                (\y -> scaleY * y)
                timeline
                lines

        Animation countdown limitsX limitsYStart limitsYEnd timeline lines ->
            let
                scaleX =
                    if limitsX.min == limitsX.max then
                        0

                    else
                        toFloat config.viewBox.width / (limitsX.max - limitsX.min)

                scaleYStart =
                    if limitsYStart.min == limitsYStart.max then
                        0

                    else
                        toFloat config.viewBox.height / (limitsYStart.max - limitsYStart.min)

                scaleYEnd =
                    if limitsYEnd.min == limitsYEnd.max then
                        0

                    else
                        toFloat config.viewBox.height / (limitsYEnd.max - limitsYEnd.min)

                scaleY =
                    scaleYStart + (scaleYEnd - scaleYStart) * easeOutQuad (1 - countdown / config.duration)
            in
            drawHelp
                (\x -> scaleX * (x - limitsX.min))
                (\y -> scaleY * y)
                timeline
                lines


minmax : List comparable -> Maybe ( comparable, comparable )
minmax =
    List.foldr
        (\point acc ->
            case acc of
                Nothing ->
                    Just ( point, point )

                Just ( minPoint, maxPoint ) ->
                    Just ( min point minPoint, max point maxPoint )
        )
        Nothing


type State
    = Empty
    | Static Limits Limits Timeline Lines
    | Animation Float Limits Limits Limits Timeline Lines


type Model
    = Model Config Data State


init : (x -> Float) -> (y -> Float) -> Float -> ViewBox -> ( Float, Float ) -> List x -> Dict String (Line (List y)) -> Model
init mapX mapY duration viewBox selector timeline lines =
    let
        config =
            Config duration viewBox

        data =
            Data
                (List.length timeline)
                (List.map mapX timeline)
                (Dict.map (\_ -> map (List.map mapY)) lines)
    in
    Model
        config
        data
        (selectHelp selector config data Empty)


select : ( Float, Float ) -> Model -> Model
select range (Model config data state) =
    Model config data (selectHelp range config data state)


consToTimeline : Float -> ( Maybe Limits, Timeline ) -> ( Maybe Limits, Timeline )
consToTimeline x ( limits, acc ) =
    ( case limits of
        Nothing ->
            Just (Limits x x)

        Just prev ->
            Just (Limits prev.min x)
    , x :: acc
    )


consToLines :
    List ( String, Float )
    -> ( Maybe Limits, Dict String (List Float) )
    -> ( Maybe Limits, Dict String (List Float) )
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
selectStep from to boundary x values acc =
    let
        approximatorLeft prevBoundary current prev =
            prev + (current - prev) * (from - prevBoundary) / (boundary - prevBoundary)

        approximatorRight prevBoundary current prev =
            prev + (current - prev) * (to - prevBoundary) / (boundary - prevBoundary)
    in
    if from <= boundary then
        if boundary <= to then
            case acc.approximation of
                ToLeft ( prevBoundary, prevX, prevValues ) ->
                    let
                        approximatedX =
                            approximatorLeft prevBoundary x prevX

                        approximatedValues =
                            approximate (approximatorLeft prevBoundary) prevValues values
                    in
                    { selectedTimeline = consToTimeline x (consToTimeline approximatedX acc.selectedTimeline)
                    , selectedValues = consToLines values (consToLines approximatedValues acc.selectedValues)
                    , approximation = ToRight ( boundary, x, List.map Tuple.second values )
                    }

                _ ->
                    { selectedTimeline = consToTimeline x acc.selectedTimeline
                    , selectedValues = consToLines values acc.selectedValues
                    , approximation = ToRight ( boundary, x, List.map Tuple.second values )
                    }

        else
            case acc.approximation of
                NotApproximate ->
                    acc

                ToLeft ( prevBoundary, prevX, prevValues ) ->
                    let
                        aproximatedLeftX =
                            approximatorLeft prevBoundary x prevX

                        aproximatedRightX =
                            approximatorRight prevBoundary x prevX

                        approximatedLeftValues =
                            approximate (approximatorLeft prevBoundary) prevValues values

                        approximatedRightValues =
                            approximate (approximatorRight prevBoundary) prevValues values
                    in
                    { selectedTimeline = List.foldr consToTimeline acc.selectedTimeline [ aproximatedRightX, aproximatedLeftX ]
                    , selectedValues = List.foldr consToLines acc.selectedValues [ approximatedRightValues, approximatedLeftValues ]
                    , approximation = NotApproximate
                    }

                ToRight ( prevBoundary, prevX, prevValues ) ->
                    let
                        approximatedX =
                            approximatorRight prevBoundary x prevX

                        approximatedValues =
                            approximate (approximatorRight prevBoundary) prevValues values
                    in
                    { selectedTimeline = consToTimeline approximatedX acc.selectedTimeline
                    , selectedValues = consToLines approximatedValues acc.selectedValues
                    , approximation = NotApproximate
                    }

    else
        { acc | approximation = ToLeft ( boundary, x, List.map Tuple.second values ) }


selectHelp : ( Float, Float ) -> Config -> Data -> State -> State
selectHelp ( from, to ) config data state =
    let
        to_ =
            clamp 0 1 to

        from_ =
            clamp 0 to_ from

        lastIndex =
            toFloat (data.size - 1)

        ( _, { selectedTimeline, selectedValues } ) =
            -- it builds timeline and Line.value in reversed order
            foldl
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
                data.timeline
                data.lines
    in
    case
        ( Dict.merge
            {- indicate a difference between input and output lineIds dicts -} (\_ _ _ -> Nothing)
            (\lineId nextValue line -> Maybe.map (Dict.insert lineId (set nextValue line)))
            {- indicate a difference between input and output lineIds dicts -} (\_ _ _ -> Nothing)
            (Tuple.second selectedValues)
            data.lines
            (Just Dict.empty)
        , Tuple.first selectedTimeline
        , Maybe.map (\limits -> Limits (min 0 limits.min) (max 0 limits.max)) (Tuple.first selectedValues)
        )
    of
        ( Just nextCorrectLines, Just limitsX, Just limitsY ) ->
            case state of
                Empty ->
                    Animation config.duration
                        limitsX
                        (Limits 0 0)
                        limitsY
                        (Tuple.second selectedTimeline)
                        nextCorrectLines

                Static _ prevLimitsY _ _ ->
                    if prevLimitsY == limitsY then
                        Static limitsX limitsY (Tuple.second selectedTimeline) nextCorrectLines

                    else
                        Animation config.duration
                            limitsX
                            prevLimitsY
                            limitsY
                            (Tuple.second selectedTimeline)
                            nextCorrectLines

                Animation countdown _ limitsYStart limitsYEnd _ _ ->
                    if limitsYEnd == limitsY then
                        Animation countdown
                            limitsX
                            limitsYStart
                            limitsYEnd
                            (Tuple.second selectedTimeline)
                            nextCorrectLines

                    else
                        let
                            done =
                                easeOutQuad (1 - countdown / config.duration)

                            deltaYStart =
                                limitsYStart.max - limitsYStart.min

                            deltaYEnd =
                                limitsYEnd.max - limitsYEnd.min
                        in
                        Animation config.duration
                            limitsX
                            { min = limitsYStart.min
                            , max = deltaYStart * deltaYEnd / (deltaYEnd + (deltaYStart - deltaYEnd) * done)
                            }
                            limitsY
                            (Tuple.second selectedTimeline)
                            nextCorrectLines

        _ ->
            Empty



-- U P D A T E


type Stage
    = Idle
    | Updated Model


type Msg
    = Tick Float


update : Msg -> Model -> Stage
update msg (Model config data state) =
    case msg of
        Tick delta ->
            case state of
                Animation countdown limitsX limitsYStart limitsYEnd timeline lines ->
                    if delta >= countdown then
                        Static limitsX limitsYEnd timeline lines
                            |> Model config data
                            |> Updated

                    else
                        Animation (countdown - delta) limitsX limitsYStart limitsYEnd timeline lines
                            |> Model config data
                            |> Updated

                _ ->
                    Idle



-- S U B S C R I P T I O N


subscriptions : Model -> Sub Msg
subscriptions (Model config data state) =
    case state of
        Animation _ _ _ _ _ _ ->
            Browser.Events.onAnimationFrameDelta Tick

        _ ->
            Sub.none



-- V I E W


makeViewBox : ViewBox -> String
makeViewBox { width, height } =
    String.join " "
        [ "0"
        , String.fromInt -height
        , String.fromInt width
        , String.fromInt height
        ]


view : Model -> Svg msg
view (Model config data state) =
    let
        chart =
            draw config state
    in
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
            (draw config state)
        )
