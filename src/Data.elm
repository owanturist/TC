module Data exposing (Chart, Line, decode)

import Json.Decode as Decode exposing (Decoder, decodeValue)
import Json.Encode as Encode exposing (Value)
import Time
import Dict exposing (Dict)

type alias Line =
    { name : String
    , color : String
    , value : List Int
    }


type alias Chart =
    { axisX : List Time.Posix
    , lines : Dict String Line
    }



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


lineDecoder : String -> Decoder Line
lineDecoder lineId =
    Decode.map3 Line
        (Decode.at [ "names", lineId ] Decode.string)
        (Decode.at [ "colors", lineId ] Decode.string)
        (Decode.at [ "columns", lineId ] (Decode.list Decode.int))


chartDecoder : Decoder Chart
chartDecoder =
    Decode.keyValuePairs Decode.string
        |> Decode.field "types"
        |> Decode.andThen
            (List.foldr
                (\( id, type_ ) acc ->
                    case type_ of
                        "x" ->
                            Decode.map2
                                (\tmp value -> { tmp | axisX = Just value })
                                acc
                                (Decode.at [ "columns", id ] (Decode.list (Decode.map Time.millisToPosix Decode.int)))

                        "line" ->
                            Decode.map2
                                (\tmp line -> { tmp | lines = ( id, line ) :: tmp.lines })
                                acc
                                (lineDecoder id)

                        unknown ->
                            Decode.fail ("Unknown type :`" ++ unknown ++ "`.")
                )
                (Decode.succeed { axisX = Nothing, lines = [] })
            )
        |> Decode.andThen
            (\acc ->
                case acc.axisX of
                    Nothing ->
                        Decode.fail "Field `x` isn't provided."

                    Just axisX ->
                        Decode.succeed (Chart axisX (Dict.fromList acc.lines))
            )


decode : Value -> Result Decode.Error Chart
decode json =
    decodeValue chartDecoder (reformat json)
