module Foo exposing (Model, Msg, Stage(..), init, select, subscriptions, update, view)

import Browser.Events
import Dict exposing (Dict)
import Svg exposing (Svg, path, svg)
import Svg.Attributes


type alias Line a =
    { name : String
    , color : String
    , value : List a
    }


map : (a -> b) -> Line a -> Line b
map fn line =
    set (List.map fn line.value) line


set : List a -> Line b -> Line a
set nextValue { name, color } =
    Line name color nextValue


type alias ViewBox =
    ( Int, Int )


type alias Config =
    { duration : Float
    , viewBox : ( Int, Int )
    }


type alias Data =
    { timeline : List Float
    , lines : Dict String (Line Float)
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


foldl : (Float -> List ( String, Float ) -> acc -> acc) -> acc -> Data -> acc
foldl fn acc { timeline, lines } =
    List.foldl
        (foldStep fn)
        ( acc, List.map (Tuple.mapSecond .value) (Dict.toList lines) )
        timeline
        |> Tuple.first


type alias Chart =
    Dict String (Dict Float Float)


dataToChart : ViewBox -> Data -> Chart
dataToChart ( _, height ) data =
    case findLinesLimits data.lines of
        Nothing ->
            Dict.empty

        Just ( minimumY, maximumY ) ->
            let
                deltaY =
                    maximumY - minimumY

                scaleY =
                    if deltaY == 0 then
                        0

                    else
                        toFloat height / deltaY
            in
            Dict.map
                (\id line ->
                    List.map2 (\x y -> ( x, scaleY * y ))
                        data.timeline
                        line.value
                        |> Dict.fromList
                )
                data.lines


draw : (x -> y -> a) -> List x -> Dict String (Line y) -> List (Line a)
draw fn timeline lines =
    List.map
        (\line -> set (List.map2 fn timeline line.value) line)
        (Dict.values lines)


easeOutQuad : Float -> Float
easeOutQuad done =
    done * (2 - done)


stateToPaths : Config -> State -> List (Line ( Float, Float ))
stateToPaths config state =
    case state of
        Initialising ->
            []

        Static timeline lines ->
            draw Tuple.pair timeline lines

        Animation countdown timeline lines ->
            let
                done =
                    1 - countdown / config.duration
            in
            draw
                (\x ( target, real ) ->
                    ( x
                    , case real of
                        Nothing ->
                            target

                        Just y ->
                            y + (target - y) * easeOutQuad done
                    )
                )
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


findLinesLimits : Dict String (Line Float) -> Maybe ( Float, Float )
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
    { minimumX : Maybe Float
    , maximumX : Maybe Float
    , minimumY : Maybe Float
    , maximumY : Maybe Float
    }


findLimits : Data -> Limits
findLimits { timeline, lines } =
    let
        timelineLimits =
            minmax timeline

        ( linesMins, linesMaxs ) =
            Dict.values lines
                |> List.filterMap (minmax << .value)
                |> List.unzip
    in
    Limits
        (Maybe.map Tuple.first timelineLimits)
        (Maybe.map Tuple.second timelineLimits)
        (List.minimum (0 :: linesMins))
        (List.maximum linesMaxs)


type State
    = Initialising
    | Static (List Float) (Dict String (Line Float))
    | Animation Float (List Float) (Dict String (Line ( Float, Maybe Float )))


type Model
    = Model Config Data State



-- type alias Bar =
--     { shiftX : Float
--     , shiftY : Float
--     , scaleX : Float
--     , scaleYFrom : Maybe Float
--     , scaleYTo : Float
--     , timeline : List Float
--     , lines : Dict String (Line Float)
--     }


init : (x -> Float) -> (y -> Float) -> Float -> ViewBox -> List x -> Dict String (Line y) -> Model
init mapX mapY duration viewBox timeline lines =
    let
        config =
            Config duration viewBox

        initialData =
            Data (List.map mapX timeline) (Dict.map (\_ -> map mapY) lines)
    in
    Model
        config
        initialData
        (foo ( 0, 1 ) config initialData Initialising)


select : ( Float, Float ) -> Model -> Model
select range (Model config data state) =
    Model config data (foo range config data state)


type Approximation a
    = NotApproximate
    | ToLeft a
    | ToRight a


approximate : (value -> value -> value) -> List value -> List ( key, value ) -> List ( key, value )
approximate approximator =
    List.map2 (\target ( key, value ) -> ( key, approximator value target ))


pushToLines : (Float -> Float) -> List ( String, Float ) -> Dict String (List ( Float, Maybe Float )) -> Dict String (List ( Float, Maybe Float ))
pushToLines bar newValues lines =
    List.foldr
        (\( key, value ) ->
            Dict.update key
                (\result ->
                    case result of
                        Nothing ->
                            Just [ ( bar value, Nothing ) ]

                        Just values ->
                            Just (( bar value, Nothing ) :: values)
                )
        )
        lines
        newValues


foo : ( Float, Float ) -> Config -> Data -> State -> State
foo ( from, to ) config data state =
    let
        to_ =
            clamp 0 1 to

        from_ =
            clamp 0 to_ from

        lastIndex =
            toFloat (List.length data.timeline - 1)

        ( width, height ) =
            config.viewBox

        limits =
            findLimits data

        scaleX =
            case Maybe.map2 (-) limits.maximumX limits.minimumX of
                Nothing ->
                    1

                Just deltaX ->
                    if deltaX == 0 then
                        0

                    else
                        toFloat width / deltaX

        scaleY =
            case Maybe.map2 (-) limits.maximumY limits.minimumY of
                Nothing ->
                    1

                Just deltaY ->
                    if deltaY == 0 then
                        0

                    else
                        toFloat height / deltaY

        shiftX =
            Maybe.withDefault 0 limits.minimumX

        barX x =
            scaleX * (x - shiftX)

        barY y =
            scaleY * -y

        tl a =
            List.map
                (\x -> scaleX * (x - shiftX))
                data.timeline

        b a =
            Dict.map
                (\_ -> map (\y -> ( scaleY * -y, Just 0 )))
                data.lines

        ( _, k ) =
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

                                        approximatedValues =
                                            approximate approximator prevValues values
                                    in
                                    { nextTimeline = barX x :: barX (approximator x prevX) :: acc.nextTimeline
                                    , nextValues = pushToLines barY values (pushToLines barY approximatedValues acc.nextValues)
                                    , approximation = ToRight ( boundary, x, List.map Tuple.second values )
                                    }

                                _ ->
                                    { nextTimeline = barX x :: acc.nextTimeline
                                    , nextValues = pushToLines barY values acc.nextValues
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

                                        approximatedLeftValues =
                                            approximate approximatorLeft prevValues values

                                        approximatedRightValues =
                                            approximate approximatorRight prevValues values
                                    in
                                    { nextTimeline = [ barX (approximatorLeft x prevX), barX (approximatorRight x prevX) ]
                                    , nextValues = List.foldr (pushToLines barY) Dict.empty [ approximatedLeftValues, approximatedRightValues ]
                                    , approximation = NotApproximate
                                    }

                                ToRight ( prevBoundary, prevX, prevValues ) ->
                                    let
                                        approximator current prev =
                                            prev + (current - prev) * (to_ - prevBoundary) / (boundary - prevBoundary)

                                        approximatedValues =
                                            approximate approximator prevValues values
                                    in
                                    { nextTimeline = barX (approximator x prevX) :: acc.nextTimeline
                                    , nextValues = pushToLines barY approximatedValues acc.nextValues
                                    , approximation = NotApproximate
                                    }

                      else
                        { acc | approximation = ToLeft ( boundary, x, List.map Tuple.second values ) }
                    )
                )
                ( 0
                , { nextTimeline = []
                  , nextValues = Dict.empty
                  , approximation = NotApproximate
                  }
                )
                data
    in
    case
        Dict.merge
            (\_ _ _ -> Nothing)
            -- indicate a difference between input and output lineIds dicts
            (\lineId line nextValue ->
                Maybe.map (Dict.insert lineId (set nextValue line))
            )
            (\_ _ _ -> Nothing)
            -- indicate a difference between input and output lineIds dicts
            data.lines
            k.nextValues
            (Just Dict.empty)
    of
        -- at least one line has been lost or new one got
        Nothing ->
            Initialising

        Just nextCorrectLines ->
            Animation config.duration k.nextTimeline nextCorrectLines



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
                Animation countdown timeline chart ->
                    if delta >= countdown then
                        Dict.map (\_ -> map Tuple.first) chart
                            |> Static timeline
                            |> Model config data
                            |> Updated

                    else
                        Animation (countdown - delta) timeline chart
                            |> Model config data
                            |> Updated

                _ ->
                    Idle



-- S U B S C R I P T I O N


subscriptions : Model -> Sub Msg
subscriptions (Model config data state) =
    case state of
        Animation _ _ _ ->
            Browser.Events.onAnimationFrameDelta Tick

        _ ->
            Sub.none



-- V I E W


makeViewBox : ViewBox -> String
makeViewBox ( width, height ) =
    String.join " "
        [ "0"
        , String.fromFloat (1.1 * toFloat -height)
        , String.fromInt width
        , String.fromFloat (1.1 * toFloat height)
        ]


coordinate : ( Float, Float ) -> String
coordinate ( x, y ) =
    String.fromFloat x ++ "," ++ String.fromFloat y


pointsToPath : List ( Float, Float ) -> String
pointsToPath points =
    case points of
        [] ->
            ""

        first :: rest ->
            List.foldl (\point acc -> acc ++ "L" ++ coordinate point)
                ("M" ++ coordinate first)
                rest


view : Model -> Svg msg
view (Model config data state) =
    let
        chart =
            stateToPaths config state
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
                    , Svg.Attributes.d (pointsToPath line.value)
                    ]
                    []
            )
            (stateToPaths config state)
        )
