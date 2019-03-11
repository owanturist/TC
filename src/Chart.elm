module Chart exposing
    ( Chart
    , decode
    , foldl
    , getColor
    , getName
    , length
    , mapX
    , mapXY
    , mapY
    , maximumX
    , maximumY
    , minimumX
    , minimumY
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, decodeValue)
import Json.Encode as Encode exposing (Value)
import Time


type alias Line y =
    { name : String
    , color : String
    , points : List y
    }


mapLine : (a -> b) -> Line a -> Line b
mapLine fn { name, color, points } =
    Line name color (List.map fn points)


type Chart x y
    = Chart (List x) (Dict String (Line y))


init : List x -> List ( String, Line y ) -> Chart x y
init axisX lines =
    let
        minLength =
            List.foldr (min << List.length << .points << Tuple.second) (List.length axisX) lines
    in
    lines
        |> List.map (Tuple.mapSecond (\line -> { line | points = List.take minLength line.points }))
        |> Dict.fromList
        |> Chart (List.take minLength axisX)


length : Chart x y -> Int
length (Chart axisX _) =
    List.length axisX


minimumX : Chart comparable y -> Maybe comparable
minimumX (Chart axisX _) =
    List.minimum axisX


maximumX : Chart comparable y -> Maybe comparable
maximumX (Chart axisX _) =
    List.maximum axisX


minimumY : Chart x comparable -> Maybe comparable
minimumY (Chart _ lines) =
    lines
        |> Dict.values
        |> List.filterMap (List.minimum << .points)
        |> List.minimum


maximumY : Chart x comparable -> Maybe comparable
maximumY (Chart _ lines) =
    lines
        |> Dict.values
        |> List.filterMap (List.maximum << .points)
        |> List.maximum


get : (Line y -> a) -> String -> Chart x y -> Maybe a
get getter lineId (Chart _ lines) =
    Maybe.map getter (Dict.get lineId lines)


getName : String -> Chart x y -> Maybe String
getName =
    get .name


getColor : String -> Chart x y -> Maybe String
getColor =
    get .color


mapX : (x -> b) -> Chart x y -> Chart b y
mapX fn (Chart axisX lines) =
    Chart (List.map fn axisX) lines


mapY : (y -> b) -> Chart x y -> Chart x b
mapY fn (Chart axisX lines) =
    Chart axisX (Dict.map (\_ -> mapLine fn) lines)


mapXY : (x -> a) -> (y -> b) -> Chart x y -> Chart a b
mapXY fnX fnY chart =
    mapY fnY (mapX fnX chart)


foldl : (x -> List ( String, y ) -> a -> a) -> a -> Chart x y -> a
foldl fn acc (Chart axisX lines) =
    let
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
    in
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



-- D E C O D I N G


reformat : Value -> Value
reformat json =
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


lineDecoder : String -> Decoder y -> Decoder (Line y)
lineDecoder lineId yDecoder =
    Decode.map3 Line
        (Decode.at [ "names", lineId ] Decode.string)
        (Decode.at [ "colors", lineId ] Decode.string)
        (Decode.at [ "columns", lineId ] (Decode.list yDecoder))


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
                                (\tmp values -> { tmp | x = Just values })
                                acc
                                (Decode.at [ "columns", id ] (Decode.list xDecoder))

                        "line" ->
                            Decode.map2
                                (\tmp line -> { tmp | lines = ( id, line ) :: tmp.lines })
                                acc
                                (lineDecoder id yDecoder)

                        unknown ->
                            Decode.fail ("Unknown type :`" ++ unknown ++ "`.")
                )
                (Decode.succeed { x = Nothing, lines = [] })
            )
        |> Decode.andThen
            (\acc ->
                case acc.x of
                    Nothing ->
                        Decode.fail "Field `x` isn't provided."

                    Just x ->
                        Decode.succeed (init x acc.lines)
            )


decode : Value -> Result Decode.Error (Chart Time.Posix Int)
decode json =
    decodeValue
        (chartDecoder
            (Decode.map Time.millisToPosix Decode.int)
            Decode.int
        )
        (reformat json)
