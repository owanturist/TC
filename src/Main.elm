module Main exposing (main)

import Browser
import Data
import Foo
import Html
import Time
import Json.Decode as Decode exposing (Value)


-- M O D E L


type alias Model =
    Foo.Model


init : Value -> ( Model, Cmd Msg )
init json =
    case Data.decode (Decode.map Time.millisToPosix Decode.int) Decode.int json of
        Err err ->
            Debug.todo (Decode.errorToString err)

        Ok chart ->
            ( Foo.init
                { animation =
                    { duration = 2000
                    }
                }
                (chart
                    |> Data.mapChartX (toFloat << Time.posixToMillis)
                    |> Data.mapChartY toFloat
                )
            , Cmd.none
            )



-- U P D A T E


type Msg
    = FooMsg Foo.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update (FooMsg msgOfFoo) model =
    ( Foo.update msgOfFoo model
    , Cmd.none
    )



-- S U B S C R I P T I O N


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map FooMsg (Foo.subscriptions model)



-- V I E W


view : Model -> Browser.Document Msg
view model =
    Browser.Document "Charts"
        [ Html.map FooMsg (Foo.view model)
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
