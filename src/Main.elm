module Main exposing (main)

import Browser
import Chart
import Data
import Html exposing (Html, code, text)
import Json.Decode as Decode exposing (Value)
import Time



-- F L A G S


type alias Flags =
    { id : String
    , data : Value
    }



-- M O D E L


type alias Model =
    Result Decode.Error Chart.Model


init : Flags -> ( Model, Cmd Msg )
init flags =
    case Data.decode (Decode.map Time.millisToPosix Decode.int) Decode.int flags.data of
        Err err ->
            ( Err err, Cmd.none )

        Ok chart ->
            Chart.init
                { id = flags.id
                , animation =
                    { duration = 500
                    , delay = 50
                    }
                }
                (chart
                    |> Data.mapChartX (toFloat << Time.posixToMillis)
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


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
