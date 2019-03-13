module Foo exposing (Model, Msg, Stage(..), init, subscriptions, update, view)

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


easeOutQuad : Float -> Float
easeOutQuad done =
    done * (2 - done)


stateToPaths : Config -> State -> List (Line ( Float, Float ))
stateToPaths config state =
    case state of
        Static timeline chart ->
            List.map
                (\line ->
                    let
                        n =
                            List.map2
                                Tuple.pair
                                timeline
                                line.value
                    in
                    set n line
                )
                (Dict.values chart)

        Animation countdown timeline chart ->
            let
                done =
                    1 - countdown / config.duration
            in
            List.map
                (\line ->
                    let
                        n =
                            List.map2
                                (\x ( y, t ) ->
                                    ( x
                                    , case t of
                                        Nothing ->
                                            y

                                        Just tmp ->
                                            tmp + (y - tmp) * easeOutQuad done
                                    )
                                )
                                timeline
                                line.value
                    in
                    set n line
                )
                (Dict.values chart)


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
    = Static (List Float) (Dict String (Line Float))
    | Animation Float (List Float) (Dict String (Line ( Float, Maybe Float )))


type Model
    = Model Config Data State


init : (x -> Float) -> (y -> Float) -> Float -> ViewBox -> List x -> Dict String (Line y) -> Model
init mapX mapY duration viewBox timeline lines =
    let
        initialData =
            Data (List.map mapX timeline) (Dict.map (\_ -> map mapY) lines)
    in
    Model
        (Config duration viewBox)
        initialData
        (foo duration viewBox ( 0, 1 ) initialData)


foo : Float -> ViewBox -> ( Float, Float ) -> Data -> State
foo duration ( width, height ) ( from, to ) data =
    let
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

        tl =
            List.map
                (\x -> scaleX * (x - shiftX))
                data.timeline

        b =
            Dict.map
                (\_ -> map (\y -> ( scaleY * -y, Just 0 )))
                data.lines
    in
    Animation duration tl b



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
                Static _ _ ->
                    Idle

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



-- S U B S C R I P T I O N


subscriptions : Model -> Sub Msg
subscriptions (Model config data state) =
    case state of
        Static _ _ ->
            Sub.none

        Animation _ _ _ ->
            Browser.Events.onAnimationFrameDelta Tick



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
