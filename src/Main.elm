module Main exposing (main)

import Browser
import Data
import Html exposing (Html, div, text)
import Task
import Time



-- M O D E L


type alias Model =
    { charts : List (Data.Chart Int)
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Model []
    , Task.perform (GenerateData << Data.generate 60) Time.now
    )



-- U P D A T E


type Msg
    = NoOp
    | GenerateData (List (Data.Chart Int))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GenerateData generatedCharts ->
            ( { model | charts = generatedCharts }
            , Cmd.none
            )



-- V I E W


view : Model -> Html Msg
view model =
    text "hello world"



-- M A I N


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view =
            \model ->
                Browser.Document "Charts"
                    [ view model
                    ]
        }
