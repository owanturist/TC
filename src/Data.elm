module Data exposing (Chart, Line, decode)

import Json.Decode as Decode exposing (Decoder, decodeValue)
import Json.Encode as Encode exposing (Value)
import Time



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


type alias Line =
    { label : String
    , color : String
    , points : List Float
    }


lineDecoder : String -> Decoder Line
lineDecoder lineId =
    Decode.map3 Line
        (Decode.at [ "names", lineId ] Decode.string)
        (Decode.at [ "colors", lineId ] Decode.string)
        (Decode.at [ "columns", lineId ] (Decode.list Decode.float))


type alias Chart =
    { x : List Time.Posix
    , lines : List Line
    }


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
                                (\tmp values -> { tmp | x = Just values })
                                acc
                                (Decode.at [ "columns", id ] (Decode.list (Decode.map Time.millisToPosix Decode.int)))

                        "line" ->
                            Decode.map2
                                (\tmp line -> { tmp | lines = line :: tmp.lines })
                                acc
                                (lineDecoder id)

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
                        Decode.succeed (Chart x acc.lines)
            )


decode : Value -> Result Decode.Error Chart
decode json =
    decodeValue chartDecoder (reformat json)
