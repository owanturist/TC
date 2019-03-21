module Data exposing
    ( Chart
    , Line
    , decode
    , filterChartLines
    , firstChartX
    , foldlChart
    , getChartLines
    , getChartTimeline
    , lastChartX
    , mapChartX
    , mapChartY
    , mapLineValue
    , setLineValue
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, decodeValue)
import Json.Encode as Encode exposing (Value)



-- L I N E


type alias Line value =
    { id : String
    , name : String
    , color : String
    , value : value
    }


mapLineValue : (a -> b) -> Line a -> Line b
mapLineValue fn { id, name, color, value } =
    Line id name color (fn value)


setLineValue : b -> Line a -> Line b
setLineValue nextValue { id, name, color } =
    Line id name color nextValue



-- C O O R D I N A T E S


type Coordinates a
    = Coordinates a a (List a)


mapCoordinates : (a -> b) -> Coordinates a -> Coordinates b
mapCoordinates fn (Coordinates first second rest) =
    Coordinates (fn first) (fn second) (List.map fn rest)


coordinatesToList : Coordinates a -> List a
coordinatesToList (Coordinates first second rest) =
    first :: second :: rest


coordinatesFromList : List a -> Maybe (Coordinates a)
coordinatesFromList list =
    case list of
        first :: second :: rest ->
            Just (Coordinates first second rest)

        _ ->
            Nothing


coordinatesDecoder : String -> Decoder a -> Decoder (Coordinates a)
coordinatesDecoder error elementDecoder =
    Decode.andThen
        (\list ->
            case coordinatesFromList list of
                Nothing ->
                    Decode.fail (error ++ " It needs to have more than one value.")

                Just coordinates ->
                    Decode.succeed coordinates
        )
        (Decode.list elementDecoder)


coordinatesLength : Coordinates a -> Int
coordinatesLength (Coordinates _ _ rest) =
    2 + List.length rest


coordinatesTake : Int -> Coordinates a -> Coordinates a
coordinatesTake count (Coordinates first second rest) =
    Coordinates first second (List.take (count - 2) rest)


coordinatesFirst : Coordinates a -> a
coordinatesFirst (Coordinates first _ _) =
    first


coordinatesLast : Coordinates a -> a
coordinatesLast (Coordinates _ second rest) =
    rest
        |> List.reverse
        |> List.head
        |> Maybe.withDefault second



-- C H A R T


type Chart x y
    = Chart (Coordinates x) (Dict String (Line (Coordinates y)))


firstChartX : Chart x y -> x
firstChartX (Chart timeline _) =
    coordinatesFirst timeline


lastChartX : Chart x y -> x
lastChartX (Chart timeline _) =
    coordinatesLast timeline


mapChartX : (a -> b) -> Chart a y -> Chart b y
mapChartX fn (Chart timeline lines) =
    Chart (mapCoordinates fn timeline) lines


mapChartY : (a -> b) -> Chart x a -> Chart x b
mapChartY fn (Chart timeline lines) =
    Chart timeline (Dict.map (\_ -> mapLineValue (mapCoordinates fn)) lines)


getChartTimeline : Chart x y -> List x
getChartTimeline (Chart timeline _) =
    coordinatesToList timeline


getChartLines : Chart x y -> Dict String (Line (Coordinates y))
getChartLines (Chart _ lines) =
    lines


filterChartLines : (String -> Line (Coordinates y) -> Bool) -> Chart x y -> Chart x y
filterChartLines fn (Chart timeline lines) =
    Chart timeline (Dict.filter fn lines)


foldChartNext : List ( key, List y ) -> Maybe ( List ( key, y ), List ( key, List y ) )
foldChartNext =
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


foldChartStep : (x -> List ( key, y ) -> acc -> acc) -> x -> ( acc, List ( key, List y ) ) -> ( acc, List ( key, List y ) )
foldChartStep fn x ( acc, values ) =
    case foldChartNext values of
        Nothing ->
            ( acc, values )

        Just ( heads, tails ) ->
            ( fn x heads acc, tails )


foldlChart : (x -> List ( String, y ) -> acc -> acc) -> acc -> Chart x y -> acc
foldlChart fn acc (Chart timeline lines) =
    List.foldl
        (foldChartStep fn)
        ( acc
        , List.map (Tuple.mapSecond (coordinatesToList << .value)) (Dict.toList lines)
        )
        (coordinatesToList timeline)
        |> Tuple.first



-- D E C O D I N G


convertColumnsFromListToDict : Value -> Value
convertColumnsFromListToDict json =
    let
        columnsDecoder =
            Decode.map2 Tuple.pair
                (Decode.index 0 Decode.string)
                (Decode.map (Encode.list identity << List.drop 1) (Decode.list Decode.value))
                |> Decode.list

        decoder =
            Decode.map4
                (\columns types names colors ->
                    [ ( "columns", Encode.object columns )
                    , ( "types", types )
                    , ( "names", names )
                    , ( "colors", colors )
                    ]
                )
                (Decode.field "columns" columnsDecoder)
                (Decode.field "types" Decode.value)
                (Decode.field "names" Decode.value)
                (Decode.field "colors" Decode.value)
    in
    case decodeValue decoder json of
        Err _ ->
            Encode.null

        Ok pairs ->
            Encode.object pairs


lineDecoder : String -> Decoder value -> Decoder (Line value)
lineDecoder lineId valueDecoder =
    Decode.map3 (Line lineId)
        (Decode.at [ "names", lineId ] Decode.string)
        (Decode.at [ "colors", lineId ] Decode.string)
        (Decode.at [ "columns", lineId ] valueDecoder)


chartDecoder : Decoder x -> Decoder y -> Decoder (Chart x y)
chartDecoder xDecoder yDecoder =
    Decode.keyValuePairs Decode.string
        |> Decode.field "types"
        |> Decode.andThen
            (List.foldr
                (\( id, type_ ) acc ->
                    case type_ of
                        "x" ->
                            Decode.map2
                                (\( _, lines ) timeline -> ( Just timeline, lines ))
                                acc
                                (Decode.at [ "columns", id ] (coordinatesDecoder "Field `x` is too short." xDecoder))

                        "line" ->
                            Decode.map2
                                (\( timeline, lines ) line -> ( timeline, ( id, line ) :: lines ))
                                acc
                                (lineDecoder id (coordinatesDecoder ("Line `" ++ id ++ "` is too short.") yDecoder))

                        unknown ->
                            Decode.fail ("Unknown type :`" ++ unknown ++ "`.")
                )
                (Decode.succeed ( Nothing, [] ))
            )
        |> Decode.andThen
            (\acc ->
                case acc of
                    ( Nothing, _ ) ->
                        Decode.fail "Field `x` isn't provided."

                    ( _, [] ) ->
                        Decode.fail "No one `line{N}` isn't provided."

                    ( Just timeline, lines ) ->
                        let
                            minLength =
                                List.foldr
                                    (\( _, line ) prev -> min prev (coordinatesLength line.value))
                                    (coordinatesLength timeline)
                                    lines
                        in
                        lines
                            |> List.map (Tuple.mapSecond (mapLineValue (coordinatesTake minLength)))
                            |> Dict.fromList
                            |> Chart (coordinatesTake minLength timeline)
                            |> Decode.succeed
            )


decode : Decoder x -> Decoder y -> Value -> Result Decode.Error (Chart x y)
decode xDecoder yDecoder json =
    decodeValue (chartDecoder xDecoder yDecoder) (convertColumnsFromListToDict json)
