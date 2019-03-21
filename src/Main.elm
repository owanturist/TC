module Main exposing (main)

import Browser
import Chart
import Data
import Html exposing (code, text)
import Json.Decode as Decode exposing (Value)
import Time



-- M O D E L


type alias Model =
    Result Decode.Error Chart.Model


init : Value -> ( Model, Cmd Msg )
init json =
    ( case Data.decode (Decode.map Time.millisToPosix Decode.int) Decode.int json of
        Err err ->
            Err err

        Ok chart ->
            Chart.init
                { id = "0"
                , animation =
                    { duration = 300
                    , delay = 50
                    }
                }
                (chart
                    |> Data.mapChartX (toFloat << Time.posixToMillis)
                    |> Data.mapChartY toFloat
                )
                |> Ok
    , Cmd.none
    )



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


view : Model -> Browser.Document Msg
view model =
    Browser.Document "Charts"
        [ case model of
            Err err ->
                code [] [ text (Decode.errorToString err) ]

            Ok chart ->
                Chart.view chart
        ]



-- M A I N


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
