module Main exposing (main)

import Browser
import Chart
import Data
import Html exposing (Html, code, text)
import Json.Decode as Decode exposing (Decoder, Value, decodeValue)



-- F L A G S


type alias Flags =
    { chart : Data.Chart Int Int
    , id : String
    , title : String
    , animation : Chart.Animation
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.map4 Flags
        (Decode.field "data"
            (Data.decoder
                Decode.int
                Decode.int
            )
        )
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.map2 Chart.Animation
            (Decode.at [ "animation", "duration" ] Decode.float
                |> Decode.maybe
                |> Decode.map (Maybe.withDefault 500)
            )
            (Decode.at [ "animation", "delay" ] Decode.float
                |> Decode.maybe
                |> Decode.map (Maybe.withDefault 50)
            )
        )



-- M O D E L


type alias Model =
    Result Decode.Error Chart.Model


init : Value -> ( Model, Cmd Msg )
init value =
    case decodeValue flagsDecoder value of
        Err err ->
            ( Err err, Cmd.none )

        Ok flags ->
            Chart.init
                { id = flags.id
                , title = flags.title
                , animation = flags.animation
                }
                (flags.chart
                    |> Data.mapChartX toFloat
                    |> Data.mapChartY toFloat
                )
                |> Tuple.mapFirst Ok



-- U P D A T E


type alias Msg =
    Chart.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msgOfChart model =
    case model of
        Err err ->
            ( Err err, Cmd.none )

        Ok chart ->
            Tuple.mapFirst Ok (Chart.update msgOfChart chart)



-- S U B S C R I P T I O N


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Err _ ->
            Sub.none

        Ok chart ->
            Chart.subscriptions chart



-- V I E W


view : Model -> Html Msg
view model =
    case model of
        Err err ->
            code [] [ text (Decode.errorToString err) ]

        Ok chart ->
            Chart.view chart



-- M A I N


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
