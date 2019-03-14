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

        Static limits timeline lines ->
            let
                scaleX =
                    if limits.minimumX == limits.maximumX then
                        0

                    else
                        toFloat config.viewBox.width / (limits.maximumX - limits.minimumX)

                scaleY =
                    if limits.minimumY == limits.maximumY then
                        0

                    else
                        toFloat config.viewBox.height / (limits.maximumY - limits.minimumY)
            in
            drawHelp
                (\x -> scaleX * (x - limits.minimumX))
                (\y -> scaleY * y)
                timeline
                lines

        Animation countdown limitsStart limitsEnd timeline lines ->
            let
                scaleX =
                    if limitsEnd.minimumX == limitsEnd.maximumX then
                        0

                    else
                        toFloat config.viewBox.width / (limitsEnd.maximumX - limitsEnd.minimumX)

                scaleStartY =
                    if limitsStart.minimumY == limitsStart.maximumY then
                        0

                    else
                        toFloat config.viewBox.height / (limitsStart.maximumY - limitsStart.minimumY)

                scaleEndY =
                    if limitsEnd.minimumY == limitsEnd.maximumY then
                        0

                    else
                        toFloat config.viewBox.height / (limitsEnd.maximumY - limitsEnd.minimumY)

                scaleY =
                    scaleStartY + (scaleEndY - scaleStartY) * easeOutQuad (1 - countdown / config.duration)
            in
            drawHelp
                (\x -> scaleX * (x - limitsEnd.minimumX))
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


findLinesLimits : Lines -> Maybe ( Float, Float )
findLinesLimits lines =
    let
        ( linesMins, linesMaxs ) =
            Dict.values lines
                |> List.filterMap (minmax << .value)
                |> List.unzip
    in
    Maybe.map2 Tuple.pair
        (List.minimum (0 :: linesMins))
        (List.maximum linesMaxs)


type alias Limits =
    { minimumX : Float
    , maximumX : Float
    , minimumY : Float
    , maximumY : Float
    }


type State
    = Empty
    | Static Limits Timeline Lines
    | Animation Float Limits Limits Timeline Lines


type Model
    = Model Config Data State


init : (x -> Float) -> (y -> Float) -> Float -> ViewBox -> ( Float, Float ) -> List x -> Dict String (Line (List y)) -> Model
init mapX mapY duration viewBox selector timeline lines =
    let
        initialData =
            Data
                (List.length timeline)
                (List.map mapX timeline)
                (Dict.map (\_ -> map (List.map mapY)) lines)
    in
    Model
        (Config duration viewBox)
        initialData
        (foo selector duration initialData Empty)


select : ( Float, Float ) -> Model -> Model
select range (Model config data state) =
    Model config data (foo range config.duration data state)


type Approximation a
    = NotApproximate
    | ToLeft a
    | ToRight a


consToTimeline : Float -> ( Maybe ( Float, Float ), Timeline ) -> ( Maybe ( Float, Float ), Timeline )
consToTimeline newX ( limits, acc ) =
    ( case limits of
        Nothing ->
            Just ( newX, newX )

        Just ( minX, _ ) ->
            Just ( minX, newX )
    , newX :: acc
    )


consToLines :
    List ( String, Float )
    -> ( Maybe ( Float, Float ), Dict String (List Float) )
    -> ( Maybe ( Float, Float ), Dict String (List Float) )
consToLines newValues prev =
    List.foldr
        (\( key, value ) ( limits, lines ) ->
            ( case limits of
                Nothing ->
                    Just ( value, value )

                Just ( minY, maxY ) ->
                    Just ( min value minY, max value maxY )
            , Dict.update key
                (\result ->
                    case result of
                        Nothing ->
                            Just [ value ]

                        Just values ->
                            Just (value :: values)
                )
                lines
            )
        )
        prev
        newValues


approximate : (value -> value -> value) -> List value -> List ( key, value ) -> List ( key, value )
approximate approximator =
    List.map2 (\target ( key, value ) -> ( key, approximator value target ))


foo : ( Float, Float ) -> Float -> Data -> State -> State
foo ( from, to ) duration data state =
    let
        to_ =
            clamp 0 1 to

        from_ =
            clamp 0 to_ from

        lastIndex =
            toFloat (data.size - 1)

        ( _, k ) =
            -- it builds timeline and Line.value in reversed order
            foldl
                (\x values ( index, acc ) ->
                    let
                        boundary =
                            index / lastIndex
                    in
                    ( index + 1
                    , if from_ <= boundary then
                        if boundary <= to_ then
                            case acc.approximation of
                                ToLeft ( prevBoundary, prevX, prevValues ) ->
                                    let
                                        approximator current prev =
                                            prev + (current - prev) * (from_ - prevBoundary) / (boundary - prevBoundary)

                                        approximatedX =
                                            approximator x prevX

                                        approximatedValues =
                                            approximate approximator prevValues values
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
                                        approximatorLeft current prev =
                                            prev + (current - prev) * (from_ - prevBoundary) / (boundary - prevBoundary)

                                        approximatorRight current prev =
                                            prev + (current - prev) * (to_ - prevBoundary) / (boundary - prevBoundary)

                                        aproximatedLeftX =
                                            approximatorLeft x prevX

                                        aproximatedRightX =
                                            approximatorRight x prevX

                                        approximatedLeftValues =
                                            approximate approximatorLeft prevValues values

                                        approximatedRightValues =
                                            approximate approximatorRight prevValues values
                                    in
                                    { selectedTimeline = List.foldr consToTimeline acc.selectedTimeline [ aproximatedRightX, aproximatedLeftX ]
                                    , selectedValues = List.foldr consToLines acc.selectedValues [ approximatedRightValues, approximatedLeftValues ]
                                    , approximation = NotApproximate
                                    }

                                ToRight ( prevBoundary, prevX, prevValues ) ->
                                    let
                                        approximator current prev =
                                            prev + (current - prev) * (to_ - prevBoundary) / (boundary - prevBoundary)

                                        approximatedX =
                                            approximator x prevX

                                        approximatedValues =
                                            approximate approximator prevValues values
                                    in
                                    { selectedTimeline = consToTimeline approximatedX acc.selectedTimeline
                                    , selectedValues = consToLines approximatedValues acc.selectedValues
                                    , approximation = NotApproximate
                                    }

                      else
                        { acc | approximation = ToLeft ( boundary, x, List.map Tuple.second values ) }
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
            (\_ _ _ -> Nothing)
            -- indicate a difference between input and output lineIds dicts
            (\lineId nextValue line -> Maybe.map (Dict.insert lineId (set nextValue line)))
            (\_ _ _ -> Nothing)
            -- indicate a difference between input and output lineIds dicts
            (Tuple.second k.selectedValues)
            data.lines
            (Just Dict.empty)
        , Tuple.first k.selectedTimeline
        , Tuple.first k.selectedValues
        )
    of
        ( Just nextCorrectLines, Just ( minimumX, maximumX ), Just ( minimumY, maximumY ) ) ->
            Animation duration
                (Limits minimumX maximumX 0 0)
                (Limits minimumX maximumX (min 0 minimumY) (max 0 maximumY))
                (Tuple.second k.selectedTimeline)
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
                Animation countdown limitsStart limitsEnd timeline chart ->
                    if delta >= countdown then
                        Static limitsEnd timeline chart
                            |> Model config data
                            |> Updated

                    else
                        Animation (countdown - delta) limitsStart limitsEnd timeline chart
                            |> Model config data
                            |> Updated

                _ ->
                    Idle



-- S U B S C R I P T I O N


subscriptions : Model -> Sub Msg
subscriptions (Model config data state) =
    case state of
        Animation _ _ _ _ _ ->
            Browser.Events.onAnimationFrameDelta Tick

        _ ->
            Sub.none



-- V I E W


makeViewBox : ViewBox -> String
makeViewBox { width, height } =
    String.join " "
        [ "0"
        , String.fromFloat (1.1 * toFloat -height)
        , String.fromInt width
        , String.fromFloat (1.1 * toFloat height)
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
                    , Svg.Attributes.strokeWidth "3"
                    , Svg.Attributes.fill "none"
                    , Svg.Attributes.d line.value
                    ]
                    []
            )
            (draw config state)
        )
