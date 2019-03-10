module Main exposing (main)

import Browser
import Data
import Html exposing (Html, div, text)
import Html.Attributes as Attributes
import Svg exposing (Svg, path, svg)
import Svg.Attributes
import Task
import Time



-- M O D E L


type alias Model =
    { selector : ( Float, Float )
    , charts : List (Data.Chart Int)
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { selector = ( 0, 1 )
      , charts = []
      }
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


pct : Float -> String
pct value =
    String.fromFloat value ++ "%"


pathCoordinate : ( Float, Float ) -> String
pathCoordinate ( x, y ) =
    String.fromFloat x ++ "," ++ String.fromFloat y


m : Bool -> ( Float, Float ) -> String
m absolute coordinate =
    if absolute then
        "M" ++ pathCoordinate coordinate

    else
        "m" ++ pathCoordinate coordinate


l : Bool -> ( Float, Float ) -> String
l absolute coordinate =
    if absolute then
        "L" ++ pathCoordinate coordinate

    else
        "l" ++ pathCoordinate coordinate


calculatePath : List ( Float, Float ) -> Maybe String
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


viewChart : Float -> Float -> Data.Chart Int -> Maybe (Svg msg)
viewChart scaleX scaleY chart =
    chart.data
        |> List.indexedMap (\i ( _, value ) -> ( scaleX * toFloat i, scaleY * toFloat value ))
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


viewCharts : Int -> Int -> List (Data.Chart Int) -> Svg msg
viewCharts width height charts =
    case
        List.foldr
            (\chart acc ->
                case ( acc, List.maximum (List.map Tuple.second chart.data) ) of
                    ( Nothing, Nothing ) ->
                        Nothing

                    ( Nothing, Just maximum ) ->
                        Just
                            { maximum = maximum
                            , size = List.length chart.data
                            }

                    ( prev, Nothing ) ->
                        prev

                    ( Just prev, Just maximum ) ->
                        Just
                            { maximum = max prev.maximum maximum
                            , size = min prev.size (List.length chart.data)
                            }
            )
            Nothing
            charts
    of
        Nothing ->
            svg
                [ Svg.Attributes.class "main__svg"
                ]
                []

        Just { maximum, size } ->
            svg
                [ Svg.Attributes.class "main__svg"
                , Svg.Attributes.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
                ]
                (List.filterMap
                    (viewChart (toFloat width / toFloat (size - 1)) (toFloat height / toFloat maximum))
                    charts
                )


viewOverviewSelector : ( Float, Float ) -> Html msg
viewOverviewSelector ( from, area ) =
    div
        [ Attributes.class "main__overview-selector"
        ]
        [ div
            [ Attributes.class "main__overview-field"
            , Attributes.style "width" (pct (100 * from))
            ]
            []
        , div
            [ Attributes.class "main__overview-field main__overview-field_active"
            , Attributes.style "width" (pct (100 * area))
            ]
            []
        , div
            [ Attributes.class "main__overview-field" ]
            []
        ]


viewOverview : ( Float, Float ) -> List (Data.Chart Int) -> Html msg
viewOverview selector charts =
    div
        [ Attributes.class "main__overview"
        ]
        [ viewCharts 460 60 charts
        , viewOverviewSelector selector
        ]


viewContainer : List (Html msg) -> Html msg
viewContainer children =
    div [ Attributes.class "main__container" ] children


view : Model -> Html Msg
view model =
    div
        [ Attributes.class "main"
        ]
        [ viewContainer
            [ viewOverview model.selector model.charts
            ]
        ]



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
