module Chart exposing (Chart, draw, init, select)

import Dict exposing (Dict)
import Time


type alias Line p =
    { name : String
    , color : String
    , points : p
    }


mapPoints : (a -> b) -> Line a -> Line b
mapPoints fn { name, color, points } =
    Line name color (fn points)


type Chart
    = Chart (List Float) (Dict String (Line (List Float)))


init : (x -> Float) -> (y -> Float) -> List x -> List ( String, Line (List y) ) -> Chart
init mapX mapY axisX lines =
    let
        minLength =
            List.foldr (min << List.length << .points << Tuple.second) (List.length axisX) lines
    in
    lines
        |> List.map (Tuple.mapSecond (mapPoints (List.map mapY << List.take minLength)))
        |> Dict.fromList
        |> Chart (List.map mapX <| List.take minLength axisX)


select : Float -> Float -> Chart -> Chart
select from to (Chart axisX lines) =
    let
        ( from_, to_ ) =
            ( max 0 from, min 1 to )

        lastIndex =
            toFloat (List.length axisX - 1)

        fox : List ( String, Float ) -> Dict String (List Float) -> Dict String (List Float)
        fox li d =
            List.foldr
                (\( lineId, y ) ->
                    Dict.update lineId
                        (\result ->
                            case result of
                                Nothing ->
                                    Just [ y ]

                                Just prev ->
                                    Just (y :: prev)
                        )
                )
                d
                li

        appl : (Float -> Float -> Float) -> List Float -> List ( String, Float ) -> List ( String, Float )
        appl shifter =
            List.map2
                (\prev ( key, next ) -> ( key, shifter next prev ))

        ( _, bar ) =
            foldr
                (\x foo ( index, acc ) ->
                    let
                        boundary =
                            1 - index / lastIndex
                    in
                    ( index + 1
                    , if from_ <= boundary then
                        if boundary <= to_ then
                            case acc.right of
                                Nothing ->
                                    { nextX = x :: acc.nextX
                                    , nextY = fox foo acc.nextY
                                    , left = Just ( boundary, x, List.map Tuple.second foo )
                                    , right = Nothing
                                    }

                                Just ( b, px, pfoo ) ->
                                    let
                                        asd next prev =
                                            next + (prev - next) * (to_ - boundary) / (b - boundary)
                                    in
                                    { nextX = x :: asd x px :: acc.nextX
                                    , nextY = fox foo (fox (appl asd pfoo foo) acc.nextY)
                                    , left = Just ( boundary, x, List.map Tuple.second foo )
                                    , right = Nothing
                                    }

                        else
                            { acc | right = Just ( boundary, x, List.map Tuple.second foo ) }

                      else
                        case acc.left of
                            Nothing ->
                                case ( acc.nextX, acc.right ) of
                                    ( [], Just ( b, px, pfoo ) ) ->
                                        let
                                            asd next prev =
                                                next + (prev - next) * (from_ - boundary) / (b - boundary)

                                            asd2 next prev =
                                                next + (prev - next) * (to_ - boundary) / (b - boundary)
                                        in
                                        { nextX = asd x px :: asd2 x px :: acc.nextX
                                        , nextY = fox (appl asd pfoo foo) (fox (appl asd2 pfoo foo) acc.nextY)
                                        , left = acc.left
                                        , right = acc.right
                                        }

                                    _ ->
                                        acc

                            Just ( b, px, pfoo ) ->
                                let
                                    asd prev next =
                                        next + (prev - next) * (b - from_) / (b - boundary)
                                in
                                { nextX = asd x px :: acc.nextX
                                , nextY = fox (appl asd pfoo foo) acc.nextY
                                , left = Nothing
                                , right = acc.right
                                }
                    )
                )
                ( 0
                , { nextX = []
                  , nextY = Dict.empty
                  , left = Nothing
                  , right = Nothing
                  }
                )
                (Chart axisX lines)

        nextLines =
            List.filterMap
                (\( lineId, line ) ->
                    Maybe.map (\nextPoints -> ( lineId, { line | points = nextPoints } )) (Dict.get lineId bar.nextY)
                )
                (Dict.toList lines)
                |> Dict.fromList
    in
    Chart bar.nextX nextLines


coordinate : Float -> Float -> String
coordinate x y =
    String.fromFloat x ++ "," ++ String.fromFloat y


m : Bool -> Float -> Float -> String
m absolute x y =
    if absolute then
        "M" ++ coordinate x y

    else
        "m" ++ coordinate x y


l : Bool -> Float -> Float -> String
l absolute x y =
    if absolute then
        "L" ++ coordinate x y

    else
        "l" ++ coordinate x y


draw : Int -> Int -> Chart -> List (Line String)
draw width height (Chart axisX lines) =
    let
        limits =
            { minX = minimumX (Chart axisX lines)
            , maxX = maximumX (Chart axisX lines)
            , minY = Maybe.map (min 0) (minimumY (Chart axisX lines))
            , maxY = maximumY (Chart axisX lines)
            }

        scaleX =
            case Maybe.map2 (-) limits.maxX limits.minX of
                Nothing ->
                    1

                Just deltaX ->
                    if deltaX == 0 then
                        0

                    else
                        toFloat width / deltaX

        scaleY =
            case Maybe.map2 (-) limits.maxY limits.minY of
                Nothing ->
                    1

                Just deltaY ->
                    if deltaY == 0 then
                        0

                    else
                        toFloat height / deltaY

        shiftX =
            Maybe.withDefault 0 limits.minX

        foo =
            foldl
                (\x points paths ->
                    List.foldr
                        (\( id, y ) ->
                            Dict.update id
                                (\path ->
                                    case path of
                                        Nothing ->
                                            Just (m True (scaleX * (x - shiftX)) (scaleY * -y))

                                        Just prev ->
                                            Just (prev ++ l True (scaleX * (x - shiftX)) (scaleY * -y))
                                )
                        )
                        paths
                        points
                )
                Dict.empty
                (Chart axisX lines)
    in
    foo
        |> Dict.toList
        |> List.filterMap (\( lineId, path ) -> Maybe.map (mapPoints (always path)) (Dict.get lineId lines))


length : Chart -> Int
length (Chart axisX _) =
    List.length axisX


minimumX : Chart -> Maybe Float
minimumX (Chart axisX _) =
    List.minimum axisX


maximumX : Chart -> Maybe Float
maximumX (Chart axisX _) =
    List.maximum axisX


minimumY : Chart -> Maybe Float
minimumY (Chart _ lines) =
    lines
        |> Dict.values
        |> List.filterMap (List.minimum << .points)
        |> List.minimum


maximumY : Chart -> Maybe Float
maximumY (Chart _ lines) =
    lines
        |> Dict.values
        |> List.filterMap (List.maximum << .points)
        |> List.maximum


get : (Line (List Float) -> a) -> String -> Chart -> Maybe a
get getter lineId (Chart _ lines) =
    Maybe.map getter (Dict.get lineId lines)


getName : String -> Chart -> Maybe String
getName =
    get .name


getColor : String -> Chart -> Maybe String
getColor =
    get .color


step : List ( String, List y ) -> Maybe ( List ( String, y ), List ( String, List y ) )
step =
    List.foldr
        (\( lineId, points ) asd ->
            case asd of
                Nothing ->
                    Nothing

                Just ( a, b ) ->
                    case points of
                        first :: rest ->
                            Just
                                ( ( lineId, first ) :: a
                                , ( lineId, rest ) :: b
                                )

                        [] ->
                            Nothing
        )
        (Just ( [], [] ))


foldl : (Float -> List ( String, Float ) -> a -> a) -> a -> Chart -> a
foldl fn acc (Chart axisX lines) =
    List.foldl
        (\x ( a, b ) ->
            case step b of
                Nothing ->
                    ( a, b )

                Just ( bunch, nextbar ) ->
                    ( fn x bunch a, nextbar )
        )
        ( acc, List.map (Tuple.mapSecond .points) (Dict.toList lines) )
        axisX
        |> Tuple.first


foldr : (Float -> List ( String, Float ) -> a -> a) -> a -> Chart -> a
foldr fn acc (Chart axisX lines) =
    List.foldr
        (\x ( a, b ) ->
            case step b of
                Nothing ->
                    ( a, b )

                Just ( bunch, nextbar ) ->
                    ( fn x bunch a, nextbar )
        )
        ( acc, List.map (Tuple.mapSecond (List.reverse << .points)) (Dict.toList lines) )
        axisX
        |> Tuple.first
