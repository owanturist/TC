module Main exposing (main)

import Browser
import Data
import Html exposing (Html, div, text)
import Svg exposing (Svg, path, svg)
import Svg.Attributes
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


pathCoordinate : ( Int, Int ) -> String
pathCoordinate ( x, y ) =
    String.fromInt x ++ "," ++ String.fromInt y


m : Bool -> ( Int, Int ) -> String
m absolute coordinate =
    if absolute then
        "M" ++ pathCoordinate coordinate

    else
        "m" ++ pathCoordinate coordinate


l : Bool -> ( Int, Int ) -> String
l absolute coordinate =
    if absolute then
        "L" ++ pathCoordinate coordinate

    else
        "l" ++ pathCoordinate coordinate


calculatePath : List ( Int, Int ) -> Maybe String
calculatePath points =
    case points of
        first :: second :: rest ->
            m True first
                :: l True second
                :: List.map (l True) rest
                |> String.join " "
                |> Just

        _ ->
            Nothing


viewChart : Data.Chart Int -> Maybe (Svg msg)
viewChart chart =
    chart.data
        |> List.indexedMap (\i ( _, value ) -> ( 10 * i, value ))
        |> calculatePath
        |> Maybe.map
            (\dValue ->
                path
                    [ Svg.Attributes.stroke chart.color
                    , Svg.Attributes.strokeWidth "1"
                    , Svg.Attributes.fill "none"
                    , Svg.Attributes.d dValue
                    ]
                    []
            )

viewCharts : List (Data.Chart Int) -> Svg msg
viewCharts charts =
    svg
        [ Svg.Attributes.viewBox "0 0 590 200"
        ]
        (List.filterMap viewChart charts)

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
                    [ viewCharts model.charts
                    ]
        }
