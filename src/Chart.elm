module Chart exposing (Chart, draw, init, select)

import Dict exposing (Dict)


type alias Line p =
    { id : String
    , name : String
    , color : String
    , value : p
    }


mapValue : (a -> b) -> Line a -> Line b
mapValue fn { id, name, color, value } =
    Line id name color (fn value)


type Chart
    = Chart (List Float) (Dict String (Line (List Float)))


init : (x -> Float) -> (y -> Float) -> List x -> Dict String (Line (List y)) -> Chart
init mapX mapY timeline lines =
    let
        minLength =
            List.foldr
                (min << List.length << .value)
                (List.length timeline)
                (Dict.values lines)

        initialTimeline =
            List.map mapX (List.take minLength timeline)

        initialLines =
            Dict.map (\_ -> mapValue (List.map mapY << List.take minLength)) lines
    in
    Chart initialTimeline initialLines


type alias Limits =
    { minimumX : Maybe Float
    , maximumX : Maybe Float
    , minimumY : Maybe Float
    , maximumY : Maybe Float
    }


findLimits : List Float -> Dict String (Line (List Float)) -> Limits
findLimits timeline lines =
    let
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


type Approximation a
    = NotApproximate
    | ToLeft a
    | ToRight a


approximate : (value -> value -> value) -> List value -> List ( key, value ) -> List ( key, value )
approximate approximator =
    List.map2 (\target ( key, value ) -> ( key, approximator value target ))


pushToLines : List ( comparable, value ) -> Dict comparable (List value) -> Dict comparable (List value)
pushToLines newValues lines =
    List.foldr
        (\( key, value ) ->
            Dict.update key
                (\result ->
                    case result of
                        Nothing ->
                            Just [ value ]

                        Just values ->
                            Just (value :: values)
                )
        )
        lines
        newValues


select : Float -> Float -> Chart -> Chart
select from to (Chart timeline lines) =
    let
        to_ =
            clamp 0 1 to

        from_ =
            clamp 0 to_ from

        lastIndex =
            toFloat (List.length timeline - 1)

        ( _, { nextTimeline, nextValues } ) =
            foldr
                (\x values ( index, acc ) ->
                    let
                        boundary =
                            1 - index / lastIndex
                    in
                    ( index + 1
                    , if from_ <= boundary then
                        if boundary <= to_ then
                            case acc.approximation of
                                ToRight ( targetBoundary, targetX, targetValues ) ->
                                    let
                                        approximator current target =
                                            current + (target - current) * (to_ - boundary) / (targetBoundary - boundary)

                                        approximatedValues =
                                            approximate approximator targetValues values
                                    in
                                    { nextTimeline = x :: approximator x targetX :: acc.nextTimeline
                                    , nextValues = pushToLines values (pushToLines approximatedValues acc.nextValues)
                                    , approximation = ToLeft ( boundary, x, List.map Tuple.second values )
                                    }

                                _ ->
                                    { nextTimeline = x :: acc.nextTimeline
                                    , nextValues = pushToLines values acc.nextValues
                                    , approximation = ToLeft ( boundary, x, List.map Tuple.second values )
                                    }

                        else
                            { acc | approximation = ToRight ( boundary, x, List.map Tuple.second values ) }

                      else
                        case acc.approximation of
                            NotApproximate ->
                                acc

                            ToLeft ( targetBoundary, targetX, targetValues ) ->
                                let
                                    approximator target current =
                                        current + (target - current) * (targetBoundary - from_) / (targetBoundary - boundary)

                                    approximatedValues =
                                        approximate approximator targetValues values
                                in
                                { nextTimeline = approximator x targetX :: acc.nextTimeline
                                , nextValues = pushToLines approximatedValues acc.nextValues
                                , approximation = NotApproximate
                                }

                            ToRight ( targetBoundary, targetX, targetValues ) ->
                                if List.isEmpty acc.nextTimeline then
                                    let
                                        approximatorLeft current target =
                                            current + (target - current) * (from_ - boundary) / (targetBoundary - boundary)

                                        approximatorRight current target =
                                            current + (target - current) * (to_ - boundary) / (targetBoundary - boundary)

                                        approximatedLeftValues =
                                            approximate approximatorLeft targetValues values

                                        approximatedRightValues =
                                            approximate approximatorRight targetValues values
                                    in
                                    { nextTimeline = [ approximatorLeft x targetX, approximatorRight x targetX ]
                                    , nextValues = List.foldr pushToLines Dict.empty [ approximatedLeftValues, approximatedRightValues ]
                                    , approximation = NotApproximate
                                    }

                                else
                                    acc
                    )
                )
                ( 0
                , { nextTimeline = []
                  , nextValues = Dict.empty
                  , approximation = NotApproximate
                  }
                )
                timeline
                lines
    in
    case
        Dict.merge
            (\_ _ _ -> Nothing)
            -- indicate a difference between input and output lineIds dicts
            (\lineId line nextValue ->
                Maybe.map (Dict.insert lineId { line | value = nextValue })
            )
            (\_ _ _ -> Nothing)
            -- indicate a difference between input and output lineIds dicts
            lines
            nextValues
            (Just Dict.empty)
    of
        -- at least one line has been lost or new one got
        Nothing ->
            Chart timeline lines

        Just nextCorrectLines ->
            Chart nextTimeline nextCorrectLines


coordinate : Float -> Float -> String
coordinate x y =
    String.fromFloat x ++ "," ++ String.fromFloat y


draw : Int -> Int -> Chart -> List (Line String)
draw width height (Chart timeline lines) =
    let
        limits =
            findLimits timeline lines

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

        makeM x y =
            "M" ++ coordinate (scaleX * (x - shiftX)) (scaleY * -y)

        makeL x y =
            "L" ++ coordinate (scaleX * (x - shiftX)) (scaleY * -y)
    in
    foldl
        (\x values acc ->
            List.foldr
                (\( id, y ) ->
                    Dict.update id
                        (\result ->
                            case result of
                                Nothing ->
                                    Maybe.map
                                        (mapValue (\_ -> makeM x y))
                                        (Dict.get id lines)

                                Just line ->
                                    Just (mapValue (\prev -> prev ++ makeL x y) line)
                        )
                )
                acc
                values
        )
        Dict.empty
        timeline
        lines
        |> Dict.values


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


foldl : (x -> List ( String, y ) -> acc -> acc) -> acc -> List x -> Dict String (Line (List y)) -> acc
foldl fn acc timeline lines =
    List.foldl
        (foldStep fn)
        ( acc, List.map (Tuple.mapSecond .value) (Dict.toList lines) )
        timeline
        |> Tuple.first


foldr : (x -> List ( String, y ) -> acc -> acc) -> acc -> List x -> Dict String (Line (List y)) -> acc
foldr fn acc timeline lines =
    List.foldr
        (foldStep fn)
        ( acc, List.map (Tuple.mapSecond (List.reverse << .value)) (Dict.toList lines) )
        timeline
        |> Tuple.first
