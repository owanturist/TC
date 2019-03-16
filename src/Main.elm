module Main exposing (main)

import Browser
import Chart
import Data
import Json.Decode as Decode exposing (Value)
import Time



-- M O D E L


type alias Model =
    Chart.Model


init : Value -> ( Model, Cmd Msg )
init json =
    case Data.decode (Decode.map Time.millisToPosix Decode.int) Decode.int json of
        Err err ->
            Debug.todo (Decode.errorToString err)

        Ok chart ->
            ( Chart.init
                { animation =
                    { duration = 300
                    }
                }
                (chart
                    |> Data.mapChartX (toFloat << Time.posixToMillis)
                    |> Data.mapChartY toFloat
                )
            , Cmd.none
            )



-- U P D A T E


type alias Msg =
    Chart.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msgOfChart model =
    ( Chart.update msgOfChart model
    , Cmd.none
    )



-- S U B S C R I P T I O N


subscriptions : Model -> Sub Msg
subscriptions model =
    Chart.subscriptions model



-- V I E W


view : Model -> Browser.Document Msg
view model =
    Browser.Document "Charts"
        [ Chart.view model
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
