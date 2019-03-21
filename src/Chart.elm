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


calcLimitsOverlap :
    ViewBox
    -> Float
    -> Limits
    -> Limits
    ->
        { min : Float
        , max : Float
        , scale : Float
        }
calcLimitsOverlap viewBox done limitsStart limitsEnd =
    let
        limitStartMin =
            calcDoneLimit limitsStart.min limitsEnd.min done

        limitStartMax =
            calcDoneLimit limitsStart.max limitsEnd.max done
    in
    { min = limitStartMin
    , max = limitStartMax
    , scale = calcScale viewBox.height (Limits limitStartMin limitStartMax)
    }


calcPointsPerFraction : Int -> Limits -> Float
calcPointsPerFraction fractionsCount limits =
    (limits.max - limits.min) / toFloat fractionsCount


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
                    , values = Nothing
                    , approximation = NoApproximate
                    }
    in
    case ( canvas, timeline, values ) of
        ( Empty, Just limitsX, Just limitsY ) ->
            if mRange == Nothing then
                Static limitsX limitsY

            else
                Static limitsX (adjustLimitsY 5 limitsY)

        ( Static _ prevLimitsY, Just limitsX, Just limitsY ) ->
            if mRange == Nothing then
                if prevLimitsY == limitsY then
                    Static limitsX limitsY

                else
                    Animated animation.delay animation.duration limitsX prevLimitsY limitsY

            else
                let
                    asd =
                        adjustLimitsY 5 limitsY
                in
                if prevLimitsY == asd then
                    Static limitsX asd

                else
                    Animated animation.delay animation.duration limitsX prevLimitsY asd

        ( Animated delay countdown _ limitsYStart limitsYEnd, Just limitsX, Just limitsY ) ->
            if mRange == Nothing then
                if limitsYEnd == limitsY then
                    Animated delay countdown limitsX limitsYStart limitsYEnd

                else
                    let
                        done =
                            easeOutQuad (1 - countdown / animation.duration)
                    in
                    Animated delay
                        animation.duration
                        limitsX
                        { min = calcDoneLimit limitsYStart.min limitsYEnd.min done
                        , max = calcDoneLimit limitsYStart.max limitsYEnd.max done
                        }
                        limitsY

            else
                let
                    asd =
                        adjustLimitsY 5 limitsY
                in
                if limitsYEnd == asd then
                    Animated delay countdown limitsX limitsYStart limitsYEnd

                else
                    let
                        done =
                            easeOutQuad (1 - countdown / animation.duration)

                        limitsYDone =
                            { min = calcDoneLimit limitsYStart.min limitsYEnd.min done
                            , max = calcDoneLimit limitsYStart.max limitsYEnd.max done
                            }
                    in
                    Animated animation.delay animation.duration limitsX limitsYDone asd

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

        Animated _ countdown limitsX limitsYStart limitsYEnd ->
            let
                scaleX =
                    calcScale viewBox.width limitsX

                done =
                    easeOutQuad (1 - countdown / animation.duration)

                overlapY =
                    calcLimitsOverlap viewBox done limitsYStart limitsYEnd
            in
            drawHelp
                (\x -> scaleX * (x - limitsX.min))
                (\y -> overlapY.scale * (overlapY.max - y))
                limitsX
                chart
                |> List.filterMap (\line -> Maybe.map (Tuple.pair line) (Dict.get line.id status))


type alias Fraction value =
    { color : String
    , opacity : Float
    , value : value
    , position : Float
    }


fractionY : Float -> Int -> Float -> Fraction Int
fractionY opacity value position =
    if value == 0 then
        Fraction "#dce3ea" opacity value position

    else
        Fraction "#f2f4f5" opacity value position


adjustBiSignLimitsY : Int -> Limits -> Limits
adjustBiSignLimitsY fractionsCount limitsY =
    let
        from =
            truncate (toFloat fractionsCount / (limitsY.max / limitsY.min + 1))
                |> clamp (1 - fractionsCount) -1

        to =
            truncate (toFloat fractionsCount / (1 - limitsY.min / limitsY.max))
                |> clamp 1 (fractionsCount - 1)

        pointsPerFractionBelowZero =
            max (limitsY.min / toFloat from) (limitsY.max / toFloat (fractionsCount + from))

        pointsPerFractionAboveZero =
            max (limitsY.min / toFloat (to - fractionsCount)) (limitsY.max / toFloat to)
    in
    if pointsPerFractionBelowZero < pointsPerFractionAboveZero then
        Limits
            (pointsPerFractionBelowZero * toFloat from)
            (pointsPerFractionBelowZero * toFloat (fractionsCount + from))

    else
        Limits
            (pointsPerFractionAboveZero * toFloat (to - fractionsCount))
            (pointsPerFractionAboveZero * toFloat to)


adjustLimitsY : Int -> Limits -> Limits
adjustLimitsY fractionsCount limitsY =
    if limitsY.min < 0 && limitsY.max > 0 then
        adjustBiSignLimitsY fractionsCount limitsY

    else
        limitsY


drawStaticFractionsY : ViewBox -> Int -> Limits -> List (Fraction Int)
drawStaticFractionsY viewBox fractionsCount limitsY =
    let
        pointsPerFraction =
            calcPointsPerFraction fractionsCount limitsY

        scaleY =
            pointsPerFraction * calcScale viewBox.height limitsY

        shiftY =
            limitsY.max / pointsPerFraction
    in
    List.map
        (\index ->
            fractionY 1
                (round ((shiftY - toFloat index) * pointsPerFraction))
                (toFloat index * scaleY)
        )
        (List.range 0 fractionsCount)



-- @TODO Fixup wrong top dirreciton


drawAnimatedFractionsY : Settings -> ViewBox -> Int -> Float -> Limits -> Limits -> List (Fraction Int)
drawAnimatedFractionsY { animation } viewBox fractionsCount countdown limitsYStart limitsYEnd =
    let
        doneEnd =
            easeOutQuad (1 - countdown / animation.duration)

        doneStart =
            1 - doneEnd

        pointsPerFractionStart =
            calcPointsPerFraction fractionsCount limitsYStart

        overlapYStart =
            calcLimitsOverlap viewBox doneStart limitsYEnd limitsYStart

        scaleYStart =
            pointsPerFractionStart * overlapYStart.scale

        shiftYStart =
            limitsYStart.max / pointsPerFractionStart

        pointsPerFractionEnd =
            calcPointsPerFraction fractionsCount limitsYEnd

        overlapYEnd =
            calcLimitsOverlap viewBox doneEnd limitsYStart limitsYEnd

        scaleYEnd =
            pointsPerFractionEnd * overlapYEnd.scale

        shiftYEnd =
            limitsYEnd.max / pointsPerFractionEnd
    in
    List.map
        (\index ->
            let
                startValue =
                    round ((shiftYStart - toFloat index) * pointsPerFractionStart)

                endValue =
                    round ((shiftYEnd - toFloat index) * pointsPerFractionEnd)
            in
            if startValue == endValue then
                [ fractionY 1 startValue (toFloat index * calcDoneLimit scaleYStart scaleYEnd doneEnd)
                ]

            else
                [ fractionY doneStart startValue (toFloat index * scaleYStart)
                , fractionY doneEnd endValue (toFloat index * scaleYEnd)
                ]
        )
        (List.range 0 fractionsCount)
        |> List.concat


drawFractionsY : Settings -> ViewBox -> Canvas -> List (Fraction Int)
drawFractionsY settings viewBox canvas =
    case canvas of
        Empty ->
            []

        Static _ limitsY ->
            drawStaticFractionsY viewBox 5 limitsY

        Animated _ countdown _ limitsYStart limitsYEnd ->
            drawAnimatedFractionsY settings viewBox 5 countdown limitsYStart limitsYEnd



-- M O D E L


type alias Limits =
    { min : Float
    , max : Float
    }


type alias Chart =
    Data.Chart Float Float


type alias Animation =
    { duration : Float
    , delay : Float
    }


type alias Settings =
    { id : String
    , animation : Animation
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
    | Animated Float Float Limits Limits Limits


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
    | ScrollCanvas Float Float
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model settings chart state) =
    let
        ( nextState, cmd ) =
            updateHelp msg settings chart state
    in
    ( Model settings chart nextState, cmd )


updateHelp : Msg -> Settings -> Chart -> State -> ( State, Cmd Msg )
updateHelp msg settings chart state =
    case msg of
        StartSelectorFromChanging start width ->
            ( { state | dragging = SelectorFromChanging state.range start width }
            , Cmd.none
            )

        StartSelectorToChanging start width ->
            ( { state | dragging = SelectorToChanging state.range start width }
            , Cmd.none
            )

        StartSelectorAreaChanging start width ->
            ( { state | dragging = SelectorAreaChanging state.range start width }
            , Cmd.none
            )

        DragSelector end ->
            ( case applyDragging state.dragging end of
                Nothing ->
                    state

                Just nextRange ->
                    { state
                        | range = nextRange
                        , canvas = select settings (Just nextRange) chart state.status state.canvas
                    }
            , Cmd.none
            )

        DragEndSelector _ ->
            ( { state | dragging = NoDragging }
            , Cmd.none
            )

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
            ( { state
                | status = nextStatus
                , minimap = select settings Nothing chart nextStatus state.minimap
                , canvas = select settings (Just state.range) chart nextStatus state.canvas
              }
            , Cmd.none
            )

        ScrollCanvas scrollTop scrollWidth ->
            let
                width =
                    state.range.to - state.range.from

                from =
                    clamp 0 (1 - width) (scrollTop / scrollWidth)

                nextRange =
                    Range from (width + from)
            in
            ( { state
                | range = nextRange
                , canvas = select settings (Just nextRange) chart state.status state.canvas
              }
            , Cmd.none
            )

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
                        Animated delay countdown limitsX limitsYStart limitsYEnd ->
                            if delta >= countdown then
                                Static limitsX limitsYEnd

                            else
                                Animated delay (countdown - delta) limitsX limitsYStart limitsYEnd

                        _ ->
                            state.minimap

                nextCanvas =
                    case state.canvas of
                        Animated delay countdown limitsX limitsYStart limitsYEnd ->
                            if delta >= countdown then
                                Static limitsX limitsYEnd

                            else if delay >= delta then
                                Animated (delay - delta) countdown limitsX limitsYStart limitsYEnd

                            else
                                Animated 0 (countdown + delay - delta) limitsX limitsYStart limitsYEnd

                        _ ->
                            state.canvas
            in
            ( { state
                | status = nextStatus
                , minimap = nextMinimap
                , canvas = nextCanvas
              }
            , Cmd.none
            )



-- S U B S C R I P T I O N


subscriptions : Model -> Sub Msg
subscriptions (Model _ _ state) =
    case state.canvas of
        Animated _ _ _ _ _ ->
            Browser.Events.onAnimationFrameDelta Tick

        _ ->
            if
                Dict.foldr
                    (\_ visibility acc ->
                        if visibility == Visible then
                            acc

                        else
                            True
                    )
                    False
                    state.status
            then
                Browser.Events.onAnimationFrameDelta Tick

            else
                Sub.none



-- V I E W


nodeID : String -> String -> String
nodeID id name =
    block ++ "__" ++ id ++ "__" ++ name


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


withScrollTopAndWith : (Float -> Float -> msg) -> Decoder msg
withScrollTopAndWith tagger =
    Decode.map2 tagger
        (Decode.field "scrollLeft" Decode.float)
        (Decode.field "scrollWidth" Decode.float)
        |> DOM.target


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


viewBreakpointsY : List (Fraction Int) -> Svg msg
viewBreakpointsY fractions =
    g
        []
        (List.map
            (\fraction ->
                path
                    [ Svg.Attributes.transform ("translate(0," ++ String.fromFloat fraction.position ++ ")")
                    , Svg.Attributes.stroke fraction.color
                    , Svg.Attributes.strokeWidth "1"
                    , Svg.Attributes.fill "none"
                    , Svg.Attributes.opacity (String.fromFloat fraction.opacity)
                    , Svg.Attributes.d ("M" ++ coordinate 0 0 ++ "L" ++ coordinate (toFloat config.viewBox.width) 0)
                    ]
                    []
            )
            fractions
        )


viewBreakpointsTextY : List (Fraction Int) -> Svg msg
viewBreakpointsTextY fractions =
    g
        []
        (List.map
            (\fraction ->
                Svg.text_
                    [ Svg.Attributes.transform ("translate(0," ++ String.fromFloat fraction.position ++ ")")
                    , Svg.Attributes.y "-8"
                    , Svg.Attributes.fontSize "14"
                    , Svg.Attributes.fontWeight "300"
                    , Svg.Attributes.fontFamily "sans-serif"
                    , Svg.Attributes.fill "#afb9c1"
                    , Svg.Attributes.opacity (String.fromFloat fraction.opacity)
                    ]
                    [ Svg.text (String.fromInt fraction.value)
                    ]
            )
            fractions
        )


viewScroller : Settings -> Range -> Html Msg
viewScroller settings range =
    div
        [ Html.Attributes.id (nodeID settings.id "scroller")
        , Html.Attributes.class (element "scroller" [])
        , Html.Events.on "scroll" (withScrollTopAndWith ScrollCanvas)
        ]
        [ div
            [ Html.Attributes.class (element "scroller-content" [])
            , Html.Attributes.style "width" (pct (100 / (range.to - range.from)))
            ]
            []
        ]


viewCanvas : Settings -> Chart -> Status -> Range -> Canvas -> Html Msg
viewCanvas settings chart status range canvas =
    let
        foos =
            drawFractionsY settings config.viewBox canvas
    in
    div
        [ Html.Attributes.class (element "canvas" [])
        ]
        [ svg
            [ Svg.Attributes.viewBox (makeViewBox config.viewBox)
            , Svg.Attributes.class (element "svg" [])
            ]
            [ viewBreakpointsY foos
            , viewLines 2 settings (draw settings config.viewBox chart status canvas)
            , viewBreakpointsTextY foos
            ]
        , viewScroller settings range
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
        [ viewCanvas settings chart state.status state.range state.canvas
        , viewContainer
            [ viewMinimap settings chart state.status state.range state.dragging state.minimap
            ]
        , viewContainer
            [ viewLinesVisibility state.status (Dict.values (Data.getChartLines chart))
            ]
        ]
