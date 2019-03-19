module Chart exposing (Model, Msg, Settings, init, subscriptions, update, view)

import Browser.Events
import DOM
import Data
import Dict exposing (Dict)
import Html exposing (Html, div, input, label, span, text)
import Html.Attributes
import Html.Events
import Html.Keyed
import Html.Lazy
import Json.Decode as Decode exposing (Decoder)
import Regex
import Svg exposing (Svg, g, path, svg)
import Svg.Attributes
import Svg.Keyed


easeOutQuad : Float -> Float
easeOutQuad delta =
    delta * (2 - delta)


calcScale : Int -> Limits -> Float
calcScale side { min, max } =
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


selectAll : Float -> List ( String, Float ) -> Selection -> Selection
selectAll x bunch acc =
    { timeline = mergeLimitX x acc.timeline
    , values = mergeLimitY bunch acc.values
    , approximation = acc.approximation
    }


selectWithRange : Range -> Float -> Float -> Float -> List ( String, Float ) -> Selection -> Selection
selectWithRange { from, to } firstX lastX x bunch acc =
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


select : Settings -> Maybe Range -> Chart -> Status -> Canvas -> Canvas
select { animation } mRange chart status canvas =
    let
        selectStep =
            case mRange of
                Nothing ->
                    selectAll

                Just range ->
                    selectWithRange
                        range
                        (Data.firstChartX chart)
                        (Data.lastChartX chart)

        { timeline, values } =
            chart
                |> Data.filterChartLines
                    (\lineId _ ->
                        Dict.get lineId status
                            |> Maybe.map isSelected
                            |> Maybe.withDefault False
                    )
                |> Data.foldlChart selectStep
                    { timeline = Nothing
                    , values = Just (Limits 0 0)
                    , approximation = NoApproximate
                    }
    in
    case ( canvas, timeline, values ) of
        ( Empty, Just limitsX, Just limitsY ) ->
            if mRange == Nothing then
                Static limitsX limitsY

            else
                Static limitsX (baz config.viewBox 5 limitsY)

        ( Static _ prevLimitsY, Just limitsX, Just limitsY ) ->
            if mRange == Nothing then
                if prevLimitsY == limitsY then
                    Static limitsX limitsY

                else
                    Animated animation.duration limitsX prevLimitsY limitsY

            else
                let
                    asd =
                        baz config.viewBox 5 limitsY
                in
                if prevLimitsY == asd then
                    Static limitsX asd

                else
                    Animated animation.duration limitsX prevLimitsY asd

        ( Animated countdown _ limitsYStart limitsYEnd, Just limitsX, Just limitsY ) ->
            if mRange == Nothing then
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
            else
                let
                    asd =
                        baz config.viewBox 5 limitsY
                in
                if limitsYEnd == asd then
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
                        asd

        _ ->
            Empty


coordinate : Float -> Float -> String
coordinate x y =
    String.fromFloat x ++ "," ++ String.fromFloat y


drawStep : (x -> Float) -> (y -> Float) -> x -> List ( String, y ) -> Dict String String -> Dict String String
drawStep mapX mapY x bunch acc =
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


drawHelp : (Float -> Float) -> (Float -> Float) -> Limits -> Chart -> List (Data.Line String)
drawHelp mapX mapY limitsX chart =
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
                            drawStep mapX mapY x bunch (drawStep mapX mapY limitsX.min bunchMin acc)

                        _ ->
                            drawStep mapX mapY x bunch acc
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
                            drawStep mapX mapY limitsX.max bunchMax (drawStep mapX mapY limitsX.min bunchMin acc)

                        ToRight ( prevX, prevBunch ) ->
                            let
                                scaleMax =
                                    (limitsX.max - prevX) / (x - prevX)

                                bunchMax =
                                    bunchBetween scaleMax prevBunch bunch
                            in
                            drawStep mapX mapY limitsX.max bunchMax acc

                        _ ->
                            acc
                    )

            else
                ( ToLeft ( x, bunch ), acc )
        )
        ( NoApproximate, Dict.empty )
        chart
        |> Tuple.second
        |> mergePathsToLines (Data.getChartLines chart)


draw : Settings -> ViewBox -> Chart -> Status -> Canvas -> List ( Data.Line String, Visibility )
draw { animation } viewBox chart status canvas =
    case canvas of
        Empty ->
            []

        Static limitsX limitsY ->
            let
                scaleX =
                    calcScale viewBox.width limitsX

                scaleY =
                    calcScale viewBox.height limitsY
            in
            chart
                |> Data.filterChartLines (\lineId _ -> Nothing /= Dict.get lineId status)
                |> drawHelp
                    (\x -> scaleX * (x - limitsX.min))
                    (\y -> scaleY * (limitsY.max - y))
                    limitsX
                |> List.filterMap (\line -> Maybe.map (Tuple.pair line) (Dict.get line.id status))

        Animated countdown limitsX limitsYStart limitsYEnd ->
            let
                scaleX =
                    calcScale viewBox.width limitsX

                done =
                    easeOutQuad (1 - countdown / animation.duration)

                limitYMin =
                    calcDoneLimit limitsYStart.min limitsYEnd.min done

                limitYMax =
                    calcDoneLimit limitsYStart.max limitsYEnd.max done

                scaleY =
                    calcScale viewBox.height (Limits limitYMin limitYMax)
            in
            drawHelp
                (\x -> scaleX * (x - limitsX.min))
                (\y -> scaleY * (limitYMax - y))
                limitsX
                chart
                |> List.filterMap (\line -> Maybe.map (Tuple.pair line) (Dict.get line.id status))


type alias Foo =
    { color : String
    , value : Int
    , breakpoint : Int
    }


foo : Int -> Int -> Foo
foo value breakpoint =
    let
        color =
            if value == 0 then
                "#dce3ea"

            else
                "#f2f4f5"
    in
    { color = color
    , value = value
    , breakpoint = breakpoint
    }


ko : Int -> Limits -> Int -> ( Float, Int, Int )
ko steps limits n =
    if n + 1 >= steps then
        ( abs limits.min / toFloat n
        , n
        , steps - n
        )

    else
        let
            s =
                max (abs limits.min / toFloat n) (abs limits.max / toFloat (steps - n))
        in
        if toFloat n * s - abs limits.min <= s && toFloat (steps - n) * s - abs limits.max <= s then
            ( s, n, steps - n )

        else
            ko steps limits (n + 1)


sign : number -> number
sign x =
    if x >= 0 then
        1

    else
        -1


baz : ViewBox -> Int -> Limits -> Limits
baz viewBox steps limits =
    let
        ( s, l, k ) =
            if limits.min == 0 then
                ( limits.max / toFloat steps, 0, steps )

            else if limits.max == 0 then
                ( limits.min / toFloat steps, steps, 0 )

            else
                ko steps limits 1

        from =
            round (sign limits.min) * l

        to =
            round (sign limits.max) * k
    in
    Limits
        (s * toFloat from)
        (s * toFloat to)


bar : ViewBox -> Int -> Limits -> List Foo
bar viewBox steps limitsY =
    let
        pps =
            (limitsY.max - limitsY.min) / toFloat steps

        from =
            floor (limitsY.min / pps)

        to =
            floor (limitsY.max / pps)

        scaleY =
            calcScale viewBox.height limitsY * pps
    in
    List.map
        (\index ->
            foo
                (index * round pps)
                (round (toFloat (to - index) * scaleY))
        )
        (List.range from to)


drawFoo :
    Settings
    -> ViewBox
    -> Chart
    -> Status
    -> Canvas
    ->
        { breakpointsY : List Foo
        , lines : List ( Data.Line String, Visibility )
        }
drawFoo { animation } viewBox chart status canvas =
    case canvas of
        Empty ->
            { breakpointsY = []
            , lines = []
            }

        Static limitsX limitsY ->
            let
                scaleX =
                    calcScale viewBox.width limitsX

                scaleY =
                    calcScale viewBox.height limitsY

                lines =
                    chart
                        |> Data.filterChartLines (\lineId _ -> Nothing /= Dict.get lineId status)
                        |> drawHelp
                            (\x -> scaleX * (x - limitsX.min))
                            (\y -> scaleY * (limitsY.max - y))
                            limitsX
                        |> List.filterMap (\line -> Maybe.map (Tuple.pair line) (Dict.get line.id status))
            in
            { breakpointsY = bar viewBox 5 limitsY
            , lines = lines
            }

        Animated countdown limitsX limitsYStart limitsYEnd ->
            let
                scaleX =
                    calcScale viewBox.width limitsX

                done =
                    easeOutQuad (1 - countdown / animation.duration)

                limitYMin =
                    calcDoneLimit limitsYStart.min limitsYEnd.min done

                limitYMax =
                    calcDoneLimit limitsYStart.max limitsYEnd.max done

                scaleY =
                    calcScale viewBox.height (Limits limitYMin limitYMax)

                lines =
                    drawHelp
                        (\x -> scaleX * (x - limitsX.min))
                        (\y -> scaleY * (limitYMax - y))
                        limitsX
                        chart
                        |> List.filterMap (\line -> Maybe.map (Tuple.pair line) (Dict.get line.id status))
            in
            { breakpointsY = bar viewBox 5 limitsYEnd
            , lines = lines
            }



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
    , minimap : Canvas
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
            Dict.map (\_ _ -> Visible) (Data.getChartLines chart)

        initialMinimap =
            select settings Nothing chart initialStatus Empty

        initialCanvas =
            select settings (Just initialRange) chart initialStatus Empty
    in
    Model settings chart (State initialRange NoDragging initialStatus initialMinimap initialCanvas)



-- U P D A T E


type Msg
    = StartSelectorFromChanging Float Float
    | StartSelectorToChanging Float Float
    | StartSelectorAreaChanging Float Float
    | DragSelector Float
    | DragEndSelector Float
    | SelectLine String
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
                            , canvas = select settings (Just nextRange) chart state.status state.canvas
                        }

        DragEndSelector _ ->
            Model settings
                chart
                { state | dragging = NoDragging }

        SelectLine lineId ->
            let
                nextStatus =
                    Dict.update lineId
                        (\result ->
                            case result of
                                Nothing ->
                                    Just (FadeIn settings.animation.duration)

                                Just Visible ->
                                    Just (FadeOut settings.animation.duration)

                                Just (FadeIn countdown) ->
                                    Just (FadeOut (settings.animation.duration - countdown))

                                Just (FadeOut countdown) ->
                                    Just (FadeIn (settings.animation.duration - countdown))
                        )
                        state.status
            in
            Model settings
                chart
                { state
                    | status = nextStatus
                    , minimap = select settings Nothing chart nextStatus state.minimap
                    , canvas = select settings (Just state.range) chart nextStatus state.canvas
                }

        Tick delta ->
            let
                nextStatus =
                    Dict.foldr
                        (\lineId visibility acc ->
                            case visibility of
                                Visible ->
                                    Dict.insert lineId Visible acc

                                FadeIn countdown ->
                                    if delta >= countdown then
                                        Dict.insert lineId Visible acc

                                    else
                                        Dict.insert lineId (FadeIn (countdown - delta)) acc

                                FadeOut countdown ->
                                    if delta >= countdown then
                                        acc

                                    else
                                        Dict.insert lineId (FadeOut (countdown - delta)) acc
                        )
                        Dict.empty
                        state.status

                nextMinimap =
                    case state.minimap of
                        Animated countdown limitsX limitsYStart limitsYEnd ->
                            if delta >= countdown then
                                Static limitsX limitsYEnd

                            else
                                Animated (countdown - delta) limitsX limitsYStart limitsYEnd

                        _ ->
                            state.minimap

                nextCanvas =
                    case state.canvas of
                        Animated countdown limitsX limitsYStart limitsYEnd ->
                            if delta >= countdown then
                                Static limitsX limitsYEnd

                            else
                                Animated (countdown - delta) limitsX limitsYStart limitsYEnd

                        _ ->
                            state.canvas
            in
            Model settings
                chart
                { state
                    | status = nextStatus
                    , minimap = nextMinimap
                    , canvas = nextCanvas
                }



-- S U B S C R I P T I O N


subscriptions : Model -> Sub Msg
subscriptions (Model _ _ state) =
    case state.canvas of
        Animated _ _ _ _ ->
            Browser.Events.onAnimationFrameDelta Tick

        _ ->
            if
                Dict.foldr
                    (\_ visibility acc ->
                        case visibility of
                            Visible ->
                                acc

                            _ ->
                                True
                    )
                    False
                    state.status
            then
                Browser.Events.onAnimationFrameDelta Tick

            else
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


viewLines : Float -> Settings -> List ( Data.Line String, Visibility ) -> Svg msg
viewLines strokeWidth { animation } paths =
    Svg.Keyed.node "g"
        []
        (List.map
            (\( line, visibility ) ->
                let
                    opacity =
                        case visibility of
                            Visible ->
                                1

                            FadeIn countdown ->
                                1 - countdown / animation.duration

                            FadeOut countdown ->
                                countdown / animation.duration
                in
                ( line.id
                , path
                    [ Svg.Attributes.stroke line.color
                    , Svg.Attributes.strokeWidth (String.fromFloat strokeWidth)
                    , Svg.Attributes.fill "none"
                    , Svg.Attributes.opacity (String.fromFloat opacity)
                    , Svg.Attributes.d line.value
                    ]
                    []
                )
            )
            paths
        )


viewBreakpointsY : List Foo -> Svg msg
viewBreakpointsY breakpoints =
    g
        []
        (List.map
            (\breakpoint ->
                g
                    [ Svg.Attributes.transform ("translate(0," ++ String.fromInt breakpoint.breakpoint ++ ")")
                    ]
                    [ path
                        [ Svg.Attributes.stroke breakpoint.color
                        , Svg.Attributes.strokeWidth "1"
                        , Svg.Attributes.fill "none"
                        , Svg.Attributes.d ("M" ++ coordinate 0 0 ++ "L" ++ coordinate (toFloat config.viewBox.width) 0)
                        ]
                        []
                    , Svg.text_
                        [ Svg.Attributes.y "-8"
                        , Svg.Attributes.fontSize "14"
                        , Svg.Attributes.fontWeight "100"
                        , Svg.Attributes.fontFamily "sans-serif"
                        , Svg.Attributes.fill "#afb9c1"
                        ]
                        [ Svg.text (String.fromInt breakpoint.value)
                        ]
                    ]
            )
            breakpoints
        )


viewCanvas : Settings -> Chart -> Status -> Canvas -> Html msg
viewCanvas settings chart status canvas =
    let
        asd =
            drawFoo settings config.viewBox chart status canvas
    in
    svg
        [ Svg.Attributes.viewBox (makeViewBox config.viewBox)
        , Svg.Attributes.class (element "svg" [])
        ]
        [ viewBreakpointsY asd.breakpointsY
        , viewLines 2 settings asd.lines
        ]


viewMinimap : Settings -> Chart -> Status -> Range -> Dragging -> Canvas -> Html Msg
viewMinimap settings chart status range dragging minimap =
    let
        viewBox =
            ViewBox config.viewBox.width 60
    in
    div
        [ Html.Attributes.class (element "minimap" [])
        ]
        [ svg
            [ Svg.Attributes.viewBox (makeViewBox viewBox)
            , Svg.Attributes.class (element "svg" [])
            ]
            [ viewLines 1 settings (draw settings viewBox chart status minimap)
            ]
        , Html.Lazy.lazy2 viewSelector range dragging
        ]


viewCheckIcon : Svg msg
viewCheckIcon =
    svg
        [ Svg.Attributes.viewBox "0 0 26 26"
        , Svg.Attributes.class (element "icon" [])
        ]
        [ path
            [ Svg.Attributes.d "m.3,14c-0.2-0.2-0.3-0.5-0.3-0.7s0.1-0.5 0.3-0.7l1.4-1.4c0.4-0.4 1-0.4 1.4,0l.1,.1 5.5,5.9c0.2,0.2 0.5,0.2 0.7,0l13.4-13.9h0.1v-8.88178e-16c0.4-0.4 1-0.4 1.4,0l1.4,1.4c0.4,0.4 0.4,1 0,1.4l0,0-16,16.6c-0.2,0.2-0.4,0.3-0.7,0.3-0.3,0-0.5-0.1-0.7-0.3l-7.8-8.4-.2-.3z"
            ]
            []
        ]


viewLineSwitcher : Bool -> Bool -> Data.Line a -> Html Msg
viewLineSwitcher onlyOneSelected selected line =
    let
        disabled =
            selected && onlyOneSelected

        indicatorAttrs =
            if disabled then
                []

            else
                [ Html.Attributes.style "background" line.color
                ]
    in
    label
        [ Html.Attributes.class (element "line-switcher" [])
        ]
        [ input
            [ Html.Attributes.class (element "line-checkbox" [])
            , Html.Attributes.type_ "checkbox"
            , Html.Attributes.disabled disabled
            , Html.Attributes.checked selected
            , Html.Events.onCheck (\_ -> SelectLine line.id)
            ]
            []
        , span
            (Html.Attributes.class (element "line-indicator" []) :: indicatorAttrs)
            [ viewCheckIcon
            ]
        , text line.name
        ]


viewLinesVisibility : Status -> List (Data.Line a) -> Html Msg
viewLinesVisibility status lines =
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
        [ Html.Attributes.class (element "legend" [])
        ]
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
            [ viewMinimap settings chart state.status state.range state.dragging state.minimap
            ]
        , viewContainer
            [ viewLinesVisibility state.status (Dict.values (Data.getChartLines chart))
            ]
        ]
