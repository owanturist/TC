module Data exposing (Chart, Line, decode, foldlChart, mapChartX, mapChartY, mapLineValue, setLineValue)

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



-- C H A R T


type alias Chart x y =
    { size : Int
    , timeline : List x
    , lines : Dict String (Line (List y))
    }


mapChartX : (a -> b) -> Chart a y -> Chart b y
mapChartX fn { size, timeline, lines } =
    Chart size (List.map fn timeline) lines


mapChartY : (a -> b) -> Chart x a -> Chart x b
mapChartY fn { size, timeline, lines } =
    Chart size timeline (Dict.map (\_ -> mapLineValue (List.map fn)) lines)


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
foldlChart fn acc { timeline, lines } =
    List.foldl
        (foldChartStep fn)
        ( acc, List.map (Tuple.mapSecond .value) (Dict.toList lines) )
        timeline
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
                                (Decode.at [ "columns", id ] (Decode.list xDecoder))

                        "line" ->
                            Decode.map2
                                (\( timeline, lines ) line -> ( timeline, ( id, line ) :: lines ))
                                acc
                                (lineDecoder id (Decode.list yDecoder))

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

                    ( Just (firstX :: []), _ ) ->
                        Decode.fail "Field `x` is too short. It needs to have more than one value."

                    ( Just timeline, lines ) ->
                        List.foldr
                            (\( lineId, line ) ->
                                Decode.andThen
                                    (\prevMin ->
                                        let
                                            lineLength =
                                                List.length line.value
                                        in
                                        if lineLength < 2 then
                                            Decode.fail ("Line `" ++ lineId ++ "` is too short. It needs to have more than one value.")

                                        else
                                            Decode.succeed (min prevMin lineLength)
                                    )
                            )
                            (Decode.succeed (List.length timeline))
                            lines
                            |> Decode.map
                                (\minLength ->
                                    lines
                                        |> List.map (Tuple.mapSecond (mapLineValue (List.take minLength)))
                                        |> Dict.fromList
                                        |> Chart minLength (List.take minLength timeline)
                                )
            )


decode : Decoder x -> Decoder y -> Value -> Result Decode.Error (Chart x y)
decode xDecoder yDecoder json =
    decodeValue (chartDecoder xDecoder yDecoder) (convertColumnsFromListToDict json)
