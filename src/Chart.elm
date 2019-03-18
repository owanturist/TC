module Chart exposing (Model, Msg, Settings, init, subscriptions, update, view)

import Browser.Events
import DOM
import Data
import Dict exposing (Dict)
import Html exposing (Html, div, input, label, text)
import Html.Attributes
import Html.Events
import Html.Keyed
import Html.Lazy
import Json.Decode as Decode exposing (Decoder)
import Regex
import Svg exposing (Svg, path, svg)
import Svg.Attributes
import Svg.Keyed


easeOutQuad : Float -> Float
easeOutQuad delta =
    delta * (2 - delta)


calcScale : Int -> Float -> Float -> Float
calcScale side min max =
    if min == max then
        0

    else
        toFloat side / (max - min)


calcDoneLimit : Float -> Float -> Float -> Float
calcDoneLimit start end done =
    start + (end - start) * done


mergePathsToLines : Dict String (Data.Line a) -> Dict String String -> List (Data.Line String)
mergePathsToLines lines paths =
    Dict.merge
        {- indicate a difference between input and output lineIds dicts -} (\_ _ _ -> Nothing)
        (\_ nextValue line -> Maybe.map ((::) (Data.setLineValue nextValue line)))
        {- indicate a difference between input and output lineIds dicts -} (\_ _ _ -> Nothing)
        paths
        lines
        (Just [])
        |> Maybe.withDefault []


approximate : (value -> value -> value) -> List value -> List ( key, value ) -> List ( key, value )
approximate approximator =
    List.map2 (\target ( key, value ) -> ( key, approximator value target ))


type Approximation a
    = NoApproximate
    | ToLeft a
    | ToRight a


type alias Selection =
    { timeline : Maybe Limits
    , values : Maybe Limits
    , approximation : Approximation ( Float, Float, List Float )
    }


mergeLimitX : Float -> Maybe Limits -> Maybe Limits
mergeLimitX x acc =
    case acc of
        Nothing ->
            Just (Limits x x)

        Just prev ->
            Just (Limits prev.min x)


mergeLimitY : List ( String, Float ) -> Maybe Limits -> Maybe Limits
mergeLimitY bunch acc =
    case acc of
        Nothing ->
            let
                values =
                    List.map Tuple.second bunch
            in
            Maybe.map2 Limits
                (List.minimum values)
                (List.maximum values)

        Just prev ->
            List.foldr
                (\( _, y ) limits -> Limits (min y limits.min) (max y limits.max))
                prev
                bunch
                |> Just


selectHelp : Range -> Float -> Float -> Float -> List ( String, Float ) -> Selection -> Selection
selectHelp { from, to } firstX lastX x bunch acc =
    let
        position =
            (x - firstX) / (lastX - firstX)

        approximatorLeft prevPosition current prev =
            prev + (current - prev) * (from - prevPosition) / (position - prevPosition)

        approximatorRight prevPosition current prev =
            prev + (current - prev) * (to - prevPosition) / (position - prevPosition)
    in
    if from <= position then
        if position <= to then
            case acc.approximation of
                ToLeft ( prevPosition, prevX, prevBunch ) ->
                    let
                        approximatedX =
                            approximatorLeft prevPosition x prevX

                        approximatedValues =
                            approximate (approximatorLeft prevPosition) prevBunch bunch
                    in
                    { timeline = mergeLimitX x (mergeLimitX approximatedX acc.timeline)
                    , values = mergeLimitY bunch (mergeLimitY approximatedValues acc.values)
                    , approximation = ToRight ( position, x, List.map Tuple.second bunch )
                    }

                _ ->
                    { timeline = mergeLimitX x acc.timeline
                    , values = mergeLimitY bunch acc.values
                    , approximation = ToRight ( position, x, List.map Tuple.second bunch )
                    }

        else
            case acc.approximation of
                NoApproximate ->
                    acc

                ToLeft ( prevPosition, prevX, prevBunch ) ->
                    let
                        aproximatedLeftX =
                            approximatorLeft prevPosition x prevX

                        aproximatedRightX =
                            approximatorRight prevPosition x prevX

                        approximatedLeftValues =
                            approximate (approximatorLeft prevPosition) prevBunch bunch

                        approximatedRightValues =
                            approximate (approximatorRight prevPosition) prevBunch bunch
                    in
                    { timeline = List.foldr mergeLimitX acc.timeline [ aproximatedRightX, aproximatedLeftX ]
                    , values = List.foldr mergeLimitY acc.values [ approximatedRightValues, approximatedLeftValues ]
                    , approximation = NoApproximate
                    }

                ToRight ( prevPosition, prevX, prevBunch ) ->
                    let
                        approximatedX =
                            approximatorRight prevPosition x prevX

                        approximatedValues =
                            approximate (approximatorRight prevPosition) prevBunch bunch
                    in
                    { timeline = mergeLimitX approximatedX acc.timeline
                    , values = mergeLimitY approximatedValues acc.values
                    , approximation = NoApproximate
                    }

    else
        { acc | approximation = ToLeft ( position, x, List.map Tuple.second bunch ) }


select : Settings -> Range -> Chart -> Status -> Canvas -> Canvas
select { animation } range chart status canvas =
    let
        visibleLines =
            Dict.filter
                (\lineId _ ->
                    Dict.get lineId status
                        |> Maybe.map isSelected
                        |> Maybe.withDefault False
                )
                chart.lines

        { timeline, values } =
            Data.foldlChart
                (selectHelp
                    range
                    (Data.firstChartX chart)
                    (Data.lastChartX chart)
                )
                { timeline = Nothing
                , values = Just (Limits 0 0)
                , approximation = NoApproximate
                }
                { chart | lines = visibleLines }
    in
    case ( canvas, timeline, values ) of
        ( Empty, Just limitsX, Just limitsY ) ->
            Static limitsX limitsY

        ( Static _ prevLimitsY, Just limitsX, Just limitsY ) ->
            if prevLimitsY == limitsY then
                Static limitsX limitsY

            else
                Animated animation.duration limitsX prevLimitsY limitsY

        ( Animated countdown _ limitsYStart limitsYEnd, Just limitsX, Just limitsY ) ->
            if limitsYEnd == limitsY then
                Animated countdown limitsX limitsYStart limitsYEnd

            else
                let
                    done =
                        easeOutQuad (1 - countdown / animation.duration)
                in
                Animated animation.duration
                    limitsX
                    { min = calcDoneLimit limitsYStart.min limitsYEnd.min done
                    , max = calcDoneLimit limitsYStart.max limitsYEnd.max done
                    }
                    limitsY

        _ ->
            Empty


coordinate : Float -> Float -> String
coordinate x y =
    String.fromFloat x ++ "," ++ String.fromFloat y


drawHelp : (x -> Float) -> (y -> Float) -> x -> List ( String, y ) -> Dict String String -> Dict String String
drawHelp mapX mapY x bunch acc =
    List.foldr
        (\( lineId, y ) ->
            Dict.update lineId
                (\result ->
                    case result of
                        Nothing ->
                            Just ("M" ++ coordinate (mapX x) (mapY y))

                        Just path ->
                            Just (path ++ "L" ++ coordinate (mapX x) (mapY y))
                )
        )
        acc
        bunch


drawSelected : (Float -> Float) -> (Float -> Float) -> Limits -> Chart -> List (Data.Line String)
drawSelected mapX mapY limitsX chart =
    let
        bunchBetween scale =
            List.map2 (\( _, prevY ) ( lineId, y ) -> ( lineId, prevY + (y - prevY) * scale ))
    in
    Data.foldlChart
        (\x bunch ( approximation, acc ) ->
            if x >= limitsX.min then
                if x <= limitsX.max then
                    ( ToRight ( x, bunch )
                    , case approximation of
                        ToLeft ( prevX, prevBunch ) ->
                            let
                                scaleMin =
                                    (limitsX.min - prevX) / (x - prevX)

                                bunchMin =
                                    bunchBetween scaleMin prevBunch bunch
                            in
                            drawHelp mapX mapY x bunch (drawHelp mapX mapY limitsX.min bunchMin acc)

                        _ ->
                            drawHelp mapX mapY x bunch acc
                    )

                else
                    ( NoApproximate
                    , case approximation of
                        ToLeft ( prevX, prevBunch ) ->
                            let
                                scaleMin =
                                    (limitsX.min - prevX) / (x - prevX)

                                scaleMax =
                                    (limitsX.max - prevX) / (x - prevX)

                                bunchMin =
                                    bunchBetween scaleMin prevBunch bunch

                                bunchMax =
                                    bunchBetween scaleMax prevBunch bunch
                            in
                            drawHelp mapX mapY limitsX.max bunchMax (drawHelp mapX mapY limitsX.min bunchMin acc)

                        ToRight ( prevX, prevBunch ) ->
                            let
                                scaleMax =
                                    (limitsX.max - prevX) / (x - prevX)

                                bunchMax =
                                    bunchBetween scaleMax prevBunch bunch
                            in
                            drawHelp mapX mapY limitsX.max bunchMax acc

                        _ ->
                            acc
                    )

            else
                ( ToLeft ( x, bunch ), acc )
        )
        ( NoApproximate, Dict.empty )
        chart
        |> Tuple.second
        |> mergePathsToLines chart.lines


drawCanvas : Settings -> Chart -> Status -> Canvas -> List (Data.Line String)
drawCanvas { animation } chart status canvas =
    case canvas of
        Empty ->
            []

        Static limitsX limitsY ->
            let
                scaleX =
                    calcScale config.viewBox.width limitsX.min limitsX.max

                scaleY =
                    calcScale config.viewBox.height limitsY.min limitsY.max

                visibleLines =
                    Dict.filter
                        (\lineId _ ->
                            Dict.get lineId status
                                |> Maybe.map isSelected
                                |> Maybe.withDefault False
                        )
                        chart.lines
            in
            drawSelected
                (\x -> scaleX * (x - limitsX.min))
                (\y -> scaleY * (limitsY.max - y))
                limitsX
                { chart | lines = visibleLines }

        Animated countdown limitsX limitsYStart limitsYEnd ->
            let
                scaleX =
                    calcScale config.viewBox.width limitsX.min limitsX.max

                done =
                    easeOutQuad (1 - countdown / animation.duration)

                limitYMin =
                    calcDoneLimit limitsYStart.min limitsYEnd.min done

                limitYMax =
                    calcDoneLimit limitsYStart.max limitsYEnd.max done

                scaleY =
                    calcScale config.viewBox.height limitYMin limitYMax
            in
            drawSelected
                (\x -> scaleX * (x - limitsX.min))
                (\y -> scaleY * (limitYMax - y))
                limitsX
                chart


drawMinimap : Chart -> List (Data.Line String)
drawMinimap chart =
    case
        Data.foldlChart
            (\x bunch -> Tuple.mapBoth (mergeLimitX x) (mergeLimitY bunch))
            ( Nothing, Just (Limits 0 0) )
            chart
    of
        ( Just limitsX, Just limitsY ) ->
            let
                scaleX =
                    calcScale config.viewBox.width limitsX.min limitsX.max

                scaleY =
                    calcScale 60 limitsY.min limitsY.max
            in
            Data.foldlChart
                (drawHelp
                    (\x -> scaleX * (x - limitsX.min))
                    (\y -> scaleY * (limitsY.max - y))
                )
                Dict.empty
                chart
                |> mergePathsToLines chart.lines

        _ ->
            []



-- M O D E L


type alias Limits =
    { min : Float
    , max : Float
    }


type alias Chart =
    Data.Chart Float Float


type alias Animation =
    { duration : Float
    }


type alias Settings =
    { animation : Animation
    }


type alias Range =
    { from : Float
    , to : Float
    }


type Dragging
    = NoDragging
    | SelectorFromChanging Range Float Float
    | SelectorToChanging Range Float Float
    | SelectorAreaChanging Range Float Float


applyDragging : Dragging -> Float -> Maybe Range
applyDragging dragging end =
    case dragging of
        NoDragging ->
            Nothing

        SelectorFromChanging { from, to } start width ->
            let
                -- keep minumun 48px (converted to pct) width for dragging
                delta =
                    clamp -from ((to - from) - 48 / width) ((end - start) / width)
            in
            Just
                { from = from + delta
                , to = to
                }

        SelectorToChanging { from, to } start width ->
            let
                -- keep minumun 48px (converted to pct) width for dragging
                delta =
                    clamp ((from - to) + 48 / width) (1 - to) ((end - start) / width)
            in
            Just
                { from = from
                , to = to + delta
                }

        SelectorAreaChanging { from, to } start width ->
            let
                delta =
                    clamp -from (1 - to) ((end - start) / width)
            in
            Just
                { from = from + delta
                , to = to + delta
                }


type Visibility
    = Visible
    | FadeIn Float
    | FadeOut Float


isSelected : Visibility -> Bool
isSelected visibility =
    case visibility of
        FadeOut _ ->
            False

        _ ->
            True


type alias Status =
    Dict String Visibility


type Canvas
    = Empty
    | Static Limits Limits
    | Animated Float Limits Limits Limits


type alias State =
    { range : Range
    , dragging : Dragging
    , status : Status
    , canvas : Canvas
    }


type Model
    = Model Settings Chart State


init : Settings -> Chart -> Model
init settings chart =
    let
        initialRange =
            Range 0 1

        initialStatus =
            Dict.map (\_ _ -> Visible) chart.lines

        initialCanvas =
            select settings initialRange chart initialStatus Empty
    in
    Model settings chart (State initialRange NoDragging initialStatus initialCanvas)



-- U P D A T E


type Msg
    = StartSelectorFromChanging Float Float
    | StartSelectorToChanging Float Float
    | StartSelectorAreaChanging Float Float
    | DragSelector Float
    | DragEndSelector Float
    | SelectLine String Bool
    | Tick Float


update : Msg -> Model -> Model
update msg (Model settings chart state) =
    case msg of
        StartSelectorFromChanging start width ->
            Model settings
                chart
                { state | dragging = SelectorFromChanging state.range start width }

        StartSelectorToChanging start width ->
            Model settings
                chart
                { state | dragging = SelectorToChanging state.range start width }

        StartSelectorAreaChanging start width ->
            Model settings
                chart
                { state | dragging = SelectorAreaChanging state.range start width }

        DragSelector end ->
            case applyDragging state.dragging end of
                Nothing ->
                    Model settings chart state

                Just nextRange ->
                    Model settings
                        chart
                        { state
                            | range = nextRange
                            , canvas = select settings nextRange chart state.status state.canvas
                        }

        DragEndSelector _ ->
            Model settings
                chart
                { state | dragging = NoDragging }

        SelectLine lineId selected ->
            let
                nextStatus =
                    if selected then
                        Dict.insert lineId Visible state.status

                    else
                        Dict.remove lineId state.status
            in
            Model settings
                chart
                { state
                    | status = nextStatus
                    , canvas = select settings state.range chart nextStatus state.canvas
                }

        Tick delta ->
            case state.canvas of
                Animated countdown limitsX limitsYStart limitsYEnd ->
                    let
                        nextCanvas =
                            if delta >= countdown then
                                Static limitsX limitsYEnd

                            else
                                Animated (countdown - delta) limitsX limitsYStart limitsYEnd
                    in
                    Model settings chart { state | canvas = nextCanvas }

                _ ->
                    Model settings chart state



-- S U B S C R I P T I O N


subscriptions : Model -> Sub Msg
subscriptions (Model _ _ state) =
    case state.canvas of
        Animated _ _ _ _ ->
            Browser.Events.onAnimationFrameDelta Tick

        _ ->
            Sub.none



-- V I E W


block : String
block =
    "c-h-a-r-t"


flag : String -> Bool -> ( String, Bool )
flag =
    Tuple.pair


element : String -> List ( String, Bool ) -> String
element name modificators =
    let
        fullName =
            block ++ "__" ++ name
    in
    List.foldr
        (\( mod, enabled ) acc ->
            if enabled then
                acc ++ " " ++ fullName ++ "_" ++ mod

            else
                acc
        )
        fullName
        modificators


type alias ViewBox =
    { width : Int
    , height : Int
    }


makeViewBox : ViewBox -> String
makeViewBox { width, height } =
    [ 0, 0, width, height ]
        |> List.map String.fromInt
        |> String.join " "


type alias Config =
    { viewBox : ViewBox
    }


config : Config
config =
    Config (ViewBox 460 460)


containsClass : String -> String -> Bool
containsClass x className =
    case
        Regex.fromStringWith
            { caseInsensitive = False
            , multiline = False
            }
            ("(^|\\s+)" ++ x ++ "($|\\s+)")
    of
        Nothing ->
            False

        Just regex ->
            Regex.contains regex className


closest : String -> Decoder node -> Decoder node
closest class decoder =
    Decode.andThen
        (\className ->
            if containsClass class className then
                decoder

            else
                DOM.parentElement (closest class decoder)
        )
        DOM.className


alwaysStop : Decoder msg -> Decoder ( msg, Bool )
alwaysStop decoder =
    Decode.map (\msg -> ( msg, True )) decoder


withTouchX : (Float -> msg) -> Decoder msg
withTouchX tagger =
    Decode.float
        |> Decode.at [ "changedTouches", "0", "pageX" ]
        |> Decode.map tagger


withTouchXandSelectorWidth : (Float -> Float -> Msg) -> Decoder Msg
withTouchXandSelectorWidth tagger =
    Decode.map2 tagger
        (Decode.field "pageX" Decode.float)
        (Decode.float
            |> Decode.field "clientWidth"
            |> closest (element "selector" [])
            |> DOM.target
        )
        |> Decode.at [ "changedTouches", "0" ]


pct : Float -> String
pct value =
    String.fromFloat value ++ "%"


viewContainer : List (Html msg) -> Html msg
viewContainer =
    div [ Html.Attributes.class (element "container" []) ]


viewSelector : Range -> Dragging -> Html Msg
viewSelector range dragging =
    let
        handlers =
            case dragging of
                NoDragging ->
                    []

                _ ->
                    [ Html.Events.on "touchmove" (withTouchX DragSelector)
                    , Html.Events.on "touchend" (withTouchX DragEndSelector)
                    ]
    in
    div
        (Html.Attributes.class (element "selector" [])
            :: handlers
        )
        [ div
            [ Html.Attributes.class (element "selector-field" [ flag "left" True ])
            , Html.Attributes.style "width" (pct (100 * range.from))
            ]
            []
        , div
            [ Html.Attributes.class (element "selector-field" [ flag "middle" True ])
            , Html.Attributes.style "width" (pct (100 * (range.to - range.from)))
            , Html.Events.on "touchstart" (withTouchXandSelectorWidth StartSelectorAreaChanging)
            ]
            [ div
                [ Html.Attributes.class (element "selector-expander" [ flag "from" True ])
                , Html.Events.stopPropagationOn "touchstart" (alwaysStop (withTouchXandSelectorWidth StartSelectorFromChanging))
                ]
                []
            , div
                [ Html.Attributes.class (element "selector-expander" [ flag "to" True ])
                , Html.Events.stopPropagationOn "touchstart" (alwaysStop (withTouchXandSelectorWidth StartSelectorToChanging))
                ]
                []
            ]
        , div
            [ Html.Attributes.class (element "selector-field" [ flag "right" True ])
            ]
            []
        ]


viewPaths : Float -> List (Data.Line String) -> Svg msg
viewPaths strokeWidth paths =
    Svg.Keyed.node "g"
        []
        (List.map
            (\line ->
                ( line.id
                , path
                    [ Svg.Attributes.stroke line.color
                    , Svg.Attributes.strokeWidth (String.fromFloat strokeWidth)
                    , Svg.Attributes.fill "none"
                    , Svg.Attributes.d line.value
                    ]
                    []
                )
            )
            paths
        )


viewCanvas : Settings -> Chart -> Status -> Canvas -> Html msg
viewCanvas settings chart status canvas =
    svg
        [ Svg.Attributes.viewBox (makeViewBox config.viewBox)
        , Svg.Attributes.class (element "svg" [])
        ]
        [ viewPaths 2 (drawCanvas settings chart status canvas)
        ]


viewMinimap : Chart -> Range -> Dragging -> Html Msg
viewMinimap chart range dragging =
    div
        [ Html.Attributes.class (element "minimap" [])
        ]
        [ svg
            [ Svg.Attributes.viewBox (makeViewBox (ViewBox config.viewBox.width 60))
            , Svg.Attributes.class (element "svg" [])
            ]
            [ viewPaths 1 (drawMinimap chart)
            ]
        , Html.Lazy.lazy2 viewSelector range dragging
        ]


viewLineSwitcher : Bool -> Bool -> Data.Line a -> Html Msg
viewLineSwitcher onlyOneSelected selected line =
    label
        []
        [ input
            [ Html.Attributes.type_ "checkbox"
            , Html.Attributes.disabled (selected && onlyOneSelected)
            , Html.Attributes.checked selected
            , Html.Events.onCheck (SelectLine line.id)
            ]
            []
        , text line.name
        ]


viewLinesVisibility : List (Data.Line a) -> Status -> Html Msg
viewLinesVisibility lines status =
    let
        ( selectedCount, linesWithSelection ) =
            List.foldr
                (\line ( count, acc ) ->
                    if
                        Dict.get line.id status
                            |> Maybe.map isSelected
                            |> Maybe.withDefault False
                    then
                        ( count + 1, ( True, line ) :: acc )

                    else
                        ( count, ( False, line ) :: acc )
                )
                ( 0, [] )
                lines
    in
    Html.Keyed.node "div"
        []
        (List.map
            (\( selected, line ) ->
                ( line.id
                , viewLineSwitcher (selectedCount < 2) selected line
                )
            )
            linesWithSelection
        )


view : Model -> Html Msg
view (Model settings chart state) =
    div
        [ Html.Attributes.class block
        ]
        [ viewCanvas settings chart state.status state.canvas
        , viewContainer
            [ viewMinimap chart state.range state.dragging
            ]
        , viewContainer
            [ viewLinesVisibility (Dict.values chart.lines) state.status
            ]
        ]
