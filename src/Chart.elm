module Chart exposing (Model, Msg, Settings, init, subscriptions, update, view)

import Browser.Dom
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
import Task
import Time


easeOutQuad : Float -> Float
easeOutQuad delta =
    delta * (2 - delta)


roundFor : Int -> Float -> Float
roundFor afterPoint value =
    let
        divider =
            toFloat (10 ^ max 0 afterPoint)
    in
    toFloat (round (value * divider)) / divider


calcScale : Int -> Limits -> Float
calcScale side { min, max } =
    if min == max then
        0

    else
        toFloat side / (max - min)


calcDone : Float -> Float -> Float -> Float
calcDone start end done =
    start + (end - start) * done


type alias Overlap =
    { min : Float
    , max : Float
    , scale : Float
    }


calcLimitsOverlap : Viewbox -> Float -> Limits -> Limits -> Overlap
calcLimitsOverlap viewbox done limitsStart limitsEnd =
    let
        limitStartMin =
            calcDone limitsStart.min limitsEnd.min done

        limitStartMax =
            calcDone limitsStart.max limitsEnd.max done
    in
    { min = limitStartMin
    , max = limitStartMax
    , scale = calcScale viewbox.height (Limits limitStartMin limitStartMax)
    }


calcPointsPerFraction : Int -> Limits -> Float
calcPointsPerFraction fractionsCount limits =
    (limits.max - limits.min) / toFloat fractionsCount


mergePathsToLines : Dict String (Data.Line a) -> Dict String String -> List (Data.Line String)
mergePathsToLines lines paths =
    let
        {- indicate a difference between input and output lineIds dicts -}
        erase _ _ _ =
            Nothing

        merge _ nextValue line =
            Maybe.map ((::) (Data.setLineValue nextValue line))
    in
    Maybe.withDefault [] (Dict.merge erase merge erase paths lines (Just []))


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


selectorAll : Float -> List ( String, Float ) -> Selection -> Selection
selectorAll x bunch acc =
    { timeline = mergeLimitX x acc.timeline
    , values = mergeLimitY bunch acc.values
    , approximation = acc.approximation
    }


selectorWithRange : Range -> Float -> Float -> Float -> List ( String, Float ) -> Selection -> Selection
selectorWithRange { from, to } firstX lastX x bunch acc =
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


selectLimits : (Float -> List ( String, Float ) -> Selection -> Selection) -> Chart -> Status -> Maybe ( Limits, Limits )
selectLimits selector chart status =
    let
        { timeline, values } =
            chart
                |> Data.filterChartLines
                    (\lineId _ ->
                        Dict.get lineId status
                            |> Maybe.map isSelected
                            |> Maybe.withDefault False
                    )
                |> Data.foldlChart selector
                    { timeline = Nothing

                    -- @TODO Set Limits 0 0
                    , values = Nothing
                    , approximation = NoApproximate
                    }
    in
    Maybe.map2 Tuple.pair timeline values


tickTransition : Settings -> Float -> Transition Limits -> Transition Limits
tickTransition settings delta transition =
    case transition of
        Stat limits ->
            Stat limits

        Del countdown limitsStart limitsEnd ->
            if delta >= countdown then
                Anim (settings.animation.duration + delta - countdown) limitsStart limitsEnd

            else
                Del (countdown - delta) limitsStart limitsEnd

        Anim countdown limitsStart limitsEnd ->
            if delta >= countdown then
                Stat limitsEnd

            else
                Anim (countdown - delta) limitsStart limitsEnd


isTransitionRun : Transition a -> Bool
isTransitionRun transition =
    case transition of
        Stat _ ->
            False

        _ ->
            True


selectTransitionX : Settings -> Limits -> Transition Limits -> Transition Limits
selectTransitionX { animation } limitsX transition =
    let
        deltaX =
            limitsX.max - limitsX.min
    in
    case transition of
        Stat prevLimitsX ->
            if prevLimitsX.max - prevLimitsX.min == deltaX then
                Stat limitsX

            else
                Anim animation.delay prevLimitsX limitsX

        Del _ limitsXStart limitsXEnd ->
            Anim animation.delay limitsXEnd limitsX

        Anim countdown limitsXStart limitsXEnd ->
            if limitsXEnd.max - limitsXEnd.min == deltaX then
                Anim countdown limitsXEnd limitsX

            else
                let
                    done =
                        easeOutQuad (1 - countdown / animation.duration)

                    doneLimits =
                        Limits
                            (calcDone limitsXStart.min limitsXEnd.min done)
                            (calcDone limitsXStart.max limitsXEnd.max done)
                in
                Anim animation.duration doneLimits limitsX


selectTransitionY : Settings -> Limits -> Transition Limits -> Transition Limits
selectTransitionY { animation } limitsY transition =
    case transition of
        Stat prevLimitsY ->
            if prevLimitsY == limitsY then
                Stat limitsY

            else
                Del animation.delay prevLimitsY limitsY

        Del countdown limitsYStart limitsYEnd ->
            if limitsYEnd == limitsY then
                Del countdown limitsYStart limitsYEnd

            else
                Del animation.delay limitsYStart limitsY

        Anim countdown limitsYStart limitsYEnd ->
            if limitsYEnd == limitsY then
                Anim countdown limitsYStart limitsYEnd

            else
                let
                    done =
                        easeOutQuad (1 - countdown / animation.duration)

                    doneLimits =
                        Limits
                            (calcDone limitsYStart.min limitsYEnd.min done)
                            (calcDone limitsYStart.max limitsYEnd.max done)
                in
                Anim animation.duration doneLimits limitsY


selectAll : Settings -> Chart -> Status -> Minimap -> Maybe Minimap
selectAll settings chart status canvas =
    Maybe.map
        (\( limitsX, limitsY ) ->
            Can limitsX (selectTransitionY settings limitsY canvas.limitsY)
        )
        (selectLimits selectorAll chart status)


selectWithRange : Settings -> Range -> Chart -> Status -> Canvs -> Maybe Canvs
selectWithRange settings range chart status canvas =
    Maybe.map
        (\( limitsX, limitsY ) ->
            Can
                (selectTransitionX settings limitsX canvas.limitsX)
                (selectTransitionY settings (adjustLimitsY limitsY) canvas.limitsY)
        )
        (selectLimits
            (selectorWithRange
                range
                (Data.firstChartX chart)
                (Data.lastChartX chart)
            )
            chart
            status
        )


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


drawStatic : Viewbox -> Chart -> Status -> Limits -> Limits -> List ( Data.Line String, Visibility )
drawStatic viewbox chart status limitsX limitsY =
    let
        scaleX =
            calcScale viewbox.width limitsX

        scaleY =
            calcScale viewbox.height limitsY
    in
    chart
        |> Data.filterChartLines (\lineId _ -> Nothing /= Dict.get lineId status)
        |> drawHelp
            (\x -> scaleX * (x - limitsX.min))
            (\y -> scaleY * (limitsY.max - y))
            limitsX
        |> List.filterMap (\line -> Maybe.map (Tuple.pair line) (Dict.get line.id status))


drawAnimated : Settings -> Viewbox -> Chart -> Status -> Float -> Limits -> Limits -> Limits -> List ( Data.Line String, Visibility )
drawAnimated { animation } viewbox chart status countdown limitsX limitsYStart limitsYEnd =
    let
        scaleX =
            calcScale viewbox.width limitsX

        done =
            easeOutQuad (1 - countdown / animation.duration)

        overlapY =
            calcLimitsOverlap viewbox done limitsYStart limitsYEnd
    in
    drawHelp
        (\x -> scaleX * (x - limitsX.min))
        (\y -> overlapY.scale * (overlapY.max - y))
        limitsX
        chart
        |> List.filterMap (\line -> Maybe.map (Tuple.pair line) (Dict.get line.id status))


drawMinimap : Settings -> Viewbox -> Chart -> Status -> Minimap -> List ( Data.Line String, Visibility )
drawMinimap settings viewbox chart status minimap =
    case minimap.limitsY of
        Stat limitsY ->
            drawStatic viewbox chart status minimap.limitsX limitsY

        Del _ limitsYStart _ ->
            drawStatic viewbox chart status minimap.limitsX limitsYStart

        Anim countdown limitsYStart limitsYEnd ->
            drawAnimated settings viewbox chart status countdown minimap.limitsX limitsYStart limitsYEnd


drawCanvas : Settings -> Viewbox -> Chart -> Status -> Canvs -> List ( Data.Line String, Visibility )
drawCanvas settings viewbox chart status canvas =
    let
        limitsX =
            case canvas.limitsX of
                Stat limits ->
                    limits

                Del _ _ limitsXEnd ->
                    limitsXEnd

                Anim _ _ limitsXEnd ->
                    limitsXEnd
    in
    case canvas.limitsY of
        Stat limitsY ->
            drawStatic viewbox chart status limitsX limitsY

        Del _ limitsYStart _ ->
            drawStatic viewbox chart status limitsX limitsYStart

        Anim countdown limitsYStart limitsYEnd ->
            drawAnimated settings viewbox chart status countdown limitsX limitsYStart limitsYEnd



-- @TODO move color directly to view


type alias Fraction value =
    { color : String
    , opacity : Float
    , value : value
    , position : Float
    }


fractionX : Float -> Int -> Float -> Fraction Time.Posix
fractionX opacity timestamp position =
    Fraction "" opacity (Time.millisToPosix timestamp) position


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


adjustLimitsY : Limits -> Limits
adjustLimitsY limitsY =
    if limitsY.min < 0 && limitsY.max > 0 then
        adjustBiSignLimitsY config.fractionsCountY limitsY

    else
        limitsY


drawStaticFractionsY : Limits -> List (Fraction Int)
drawStaticFractionsY limitsY =
    let
        pointsPerFraction =
            calcPointsPerFraction config.fractionsCountY limitsY

        scaleY =
            pointsPerFraction * calcScale config.viewbox.height limitsY

        shiftY =
            limitsY.max / pointsPerFraction
    in
    List.map
        (\index ->
            fractionY 1
                (round ((shiftY - toFloat index) * pointsPerFraction))
                (toFloat index * scaleY)
        )
        (List.range 0 config.fractionsCountY)



-- @TODO Fixup wrong top dirreciton


drawAnimatedFractionsY : Settings -> Float -> Limits -> Limits -> List (Fraction Int)
drawAnimatedFractionsY { animation } countdown limitsYStart limitsYEnd =
    let
        doneEnd =
            easeOutQuad (1 - countdown / animation.duration)

        doneStart =
            1 - doneEnd

        pointsPerFractionStart =
            calcPointsPerFraction config.fractionsCountY limitsYStart

        overlapYStart =
            calcLimitsOverlap config.viewbox doneStart limitsYEnd limitsYStart

        scaleYStart =
            pointsPerFractionStart * overlapYStart.scale

        shiftYStart =
            limitsYStart.max / pointsPerFractionStart

        pointsPerFractionEnd =
            calcPointsPerFraction config.fractionsCountY limitsYEnd

        overlapYEnd =
            calcLimitsOverlap config.viewbox doneEnd limitsYStart limitsYEnd

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
                [ fractionY 1 startValue (toFloat index * calcDone scaleYStart scaleYEnd doneEnd)
                ]

            else
                [ fractionY doneStart startValue (toFloat index * scaleYStart)
                , fractionY doneEnd endValue (toFloat index * scaleYEnd)
                ]
        )
        (List.range 0 config.fractionsCountY)
        |> List.concat


drawFractionsY : Settings -> Canvs -> List (Fraction Int)
drawFractionsY settings canvas =
    case canvas.limitsY of
        Stat limitsY ->
            drawStaticFractionsY limitsY

        Del _ limitsYStart _ ->
            drawStaticFractionsY limitsYStart

        Anim countdown limitsYStart limitsYEnd ->
            drawAnimatedFractionsY settings countdown limitsYStart limitsYEnd


drawStaticFractionsXHelp : Float -> Float -> List Float -> Limits -> List (Fraction Time.Posix)
drawStaticFractionsXHelp first last middle limitsX =
    let
        middleLength =
            List.length middle

        pointsPerFraction =
            calcPointsPerFraction config.fractionsCountX limitsX

        each =
            ceiling (toFloat middleLength * pointsPerFraction / (last - first))

        deltaX =
            limitsX.max - limitsX.min

        ( startIndex, normalizedMiddle ) =
            if remainderBy each middleLength <= each // 2 then
                ( each // 2
                , middle
                    |> List.take (middleLength - each // 2)
                    |> List.drop (each - each // 2)
                )

            else
                ( 1, middle )

        ( _, restFractions ) =
            List.foldr
                (\timestamp ( index, acc ) ->
                    ( index + 1
                    , if remainderBy each index == 0 then
                        timestamp :: acc

                      else
                        acc
                    )
                )
                ( startIndex, [ first ] )
                normalizedMiddle

        ( _, fractions ) =
            List.foldr
                (\timestamp ( neighbour, acc ) ->
                    if timestamp >= limitsX.min then
                        if timestamp <= limitsX.max then
                            ( Nothing
                            , case neighbour of
                                Just leftTimestamp ->
                                    fractionX 1 (round leftTimestamp) ((leftTimestamp - limitsX.min) / deltaX)
                                        :: fractionX 1 (round timestamp) ((timestamp - limitsX.min) / deltaX)
                                        :: acc

                                Nothing ->
                                    fractionX 1 (round timestamp) ((timestamp - limitsX.min) / deltaX)
                                        :: acc
                            )

                        else
                            ( Just timestamp
                            , if neighbour == Nothing then
                                fractionX 1 (round timestamp) ((timestamp - limitsX.min) / deltaX) :: acc

                              else
                                acc
                            )

                    else
                        ( Just timestamp
                        , acc
                        )
                )
                ( Nothing, [] )
                (last :: restFractions)
    in
    fractions


drawStaticFractionsX : Chart -> Limits -> List (Fraction Time.Posix)
drawStaticFractionsX chart limitsX =
    case Data.getChartTimeline chart of
        [] ->
            []

        first :: tail ->
            case List.reverse tail of
                [] ->
                    []

                last :: middle ->
                    drawStaticFractionsXHelp first last middle limitsX


drawAnimatedFractionsX : Settings -> Chart -> Float -> Limits -> Limits -> List (Fraction Time.Posix)
drawAnimatedFractionsX settings chart countdown limitsXStart limitsXEnd =
    []


drawFractionsX : Settings -> Chart -> Canvs -> List (Fraction Time.Posix)
drawFractionsX settings chart canvas =
    case canvas.limitsX of
        Stat limitsX ->
            drawStaticFractionsX chart limitsX

        Del _ _ limitsXEnd ->
            drawStaticFractionsX chart limitsXEnd

        Anim countdown limitsXStart limitsXEnd ->
            -- drawAnimatedFractionsX settings chart countdown limitsXStart limitsXEnd
            drawStaticFractionsX chart limitsXEnd



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



-- @TODO Make a better name


type alias Status =
    Dict String Visibility


type Transition a
    = Stat a
    | Del Float a a
    | Anim Float a a



-- @TODO Make better name


type alias Can x y =
    { limitsX : x
    , limitsY : y
    }



-- @TODO Make better name


type alias Canvs =
    Can (Transition Limits) (Transition Limits)


type alias Minimap =
    Can Limits (Transition Limits)


type alias Viewport =
    { width : Float
    , height : Float
    }


type alias State =
    { viewport : Maybe Viewport
    , dragging : Dragging
    , range : Range
    , status : Status
    , minimap : Minimap
    , canvs : Canvs
    }


type Model
    = Model Settings Chart State


init : Settings -> Chart -> ( Model, Cmd Msg )
init settings chart =
    let
        initialStatus =
            Dict.map (\_ _ -> Visible) (Data.getChartLines chart)

        initialMinimap =
            Can (Limits 0 0) (Stat (Limits 0 0))

        initialCanvs =
            Can (Stat (Limits 0 0)) (Stat (Limits 0 0))
    in
    ( State Nothing NoDragging (Range 0 1) initialStatus initialMinimap initialCanvs
        |> Model settings chart
    , getViewport settings
    )


getViewport : Settings -> Cmd Msg
getViewport settings =
    Task.attempt
        (\result ->
            case result of
                Err err ->
                    GetViewport (Err err)

                Ok { viewport } ->
                    GetViewport (Ok (Viewport viewport.width viewport.height))
        )
        (Browser.Dom.getViewportOf (nodeID settings.id "root"))



-- U P D A T E


type Msg
    = GetViewport (Result Browser.Dom.Error Viewport)
    | Resize Int Int
    | StartSelectorFromChanging Float Float
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
        GetViewport (Err err) ->
            ( state, Cmd.none )

        GetViewport (Ok nextViewport) ->
            ( case state.viewport of
                Nothing ->
                    { state
                        | viewport = Just nextViewport
                        , minimap = Maybe.withDefault state.minimap (selectAll settings chart state.status state.minimap)
                        , canvs = Maybe.withDefault state.canvs (selectWithRange settings state.range chart state.status state.canvs)
                    }

                Just _ ->
                    { state | viewport = Just nextViewport }
            , Cmd.none
            )

        Resize _ _ ->
            ( state, getViewport settings )

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
                        , canvs = Maybe.withDefault state.canvs (selectWithRange settings nextRange chart state.status state.canvs)
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
                , minimap = Maybe.withDefault state.minimap (selectAll settings chart nextStatus state.minimap)
                , canvs = Maybe.withDefault state.canvs (selectWithRange settings state.range chart nextStatus state.canvs)
              }
            , Cmd.none
            )

        ScrollCanvas scrollTop scrollWidth ->
            ( state, Cmd.none )

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
            in
            ( { state
                | status = nextStatus
                , minimap = Can state.minimap.limitsX (tickTransition settings delta state.minimap.limitsY)
                , canvs = Can (tickTransition settings delta state.canvs.limitsX) (tickTransition settings delta state.canvs.limitsY)
              }
            , Cmd.none
            )



-- S U B S C R I P T I O N


subscriptions : Model -> Sub Msg
subscriptions (Model _ _ state) =
    Sub.batch
        [ Browser.Events.onResize Resize
        , if
            isTransitionRun state.canvs.limitsX
                || isTransitionRun state.canvs.limitsY
                || isTransitionRun state.minimap.limitsY
                || List.any ((/=) Visible) (Dict.values state.status)
          then
            Browser.Events.onAnimationFrameDelta Tick

          else
            Sub.none
        ]



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


type alias Viewbox =
    { width : Int
    , height : Int
    }


makeViewBox : Viewbox -> String
makeViewBox { width, height } =
    [ 0, 0, width, height ]
        |> List.map String.fromInt
        |> String.join " "


type alias Config =
    { viewbox : Viewbox
    , fractionsCountX : Int
    , fractionsCountY : Int
    }


config : Config
config =
    Config (Viewbox 460 460) 5 5


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


px : Float -> String
px value =
    String.fromFloat value ++ "px"


pct : Float -> String
pct value =
    String.fromFloat (100 * roundFor 4 value) ++ "%"


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
            , Html.Attributes.style "width" (pct range.from)
            ]
            []
        , div
            [ Html.Attributes.class (element "selector-field" [ flag "middle" True ])
            , Html.Attributes.style "width" (pct (range.to - range.from))
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


monthToDateString : Time.Month -> String
monthToDateString month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


posixToDateString : Time.Zone -> Time.Posix -> String
posixToDateString zone posix =
    monthToDateString (Time.toMonth zone posix) ++ " " ++ String.fromInt (Time.toDay zone posix)


viewFractionsX : List (Fraction Time.Posix) -> Html msg
viewFractionsX fractions =
    let
        fractionWidth =
            1 / toFloat (List.length fractions)
    in
    div
        []
        (List.map
            (\fraction ->
                span
                    [ Html.Attributes.class (element "fraction-x" [])
                    , Html.Attributes.style "left" (pct fraction.position)
                    , Html.Attributes.style "width" (pct fractionWidth)
                    , Html.Attributes.style "opacity" (String.fromFloat fraction.opacity)
                    ]
                    [ text (posixToDateString Time.utc fraction.value)
                    ]
            )
            fractions
        )


viewFractionsY : List (Fraction Int) -> Svg msg
viewFractionsY fractions =
    g
        []
        (List.map
            (\fraction ->
                path
                    [ Svg.Attributes.transform ("translate(" ++ coordinate 0 fraction.position ++ ")")
                    , Svg.Attributes.stroke fraction.color
                    , Svg.Attributes.strokeWidth "1"
                    , Svg.Attributes.fill "none"
                    , Svg.Attributes.opacity (String.fromFloat fraction.opacity)
                    , Svg.Attributes.d ("M" ++ coordinate 0 0 ++ "L" ++ coordinate (toFloat config.viewbox.width) 0)
                    ]
                    []
            )
            fractions
        )


viewFractionsTextY : List (Fraction Int) -> Svg msg
viewFractionsTextY fractions =
    g
        []
        (List.map
            (\fraction ->
                Svg.text_
                    [ Svg.Attributes.transform ("translate(" ++ coordinate 0 fraction.position ++ ")")
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
            , Html.Attributes.style "width" (pct (1 / (range.to - range.from)))
            ]
            []
        ]


viewCanvas : Settings -> Chart -> Status -> Range -> Canvs -> Html Msg
viewCanvas settings chart status range canvas =
    let
        fractionsX =
            drawFractionsX settings chart canvas

        fractionsY =
            drawFractionsY settings canvas
    in
    div
        [ Html.Attributes.class (element "canvas" [])
        ]
        [ svg
            [ Svg.Attributes.viewBox (makeViewBox config.viewbox)
            , Svg.Attributes.class (element "svg" [])
            ]
            [ viewFractionsY fractionsY
            , viewLines 2 settings (drawCanvas settings config.viewbox chart status canvas)
            , viewFractionsTextY fractionsY
            ]
        , viewFractionsX fractionsX
        , viewScroller settings range
        ]


viewMinimap : Settings -> Chart -> Status -> Range -> Dragging -> Minimap -> Html Msg
viewMinimap settings chart status range dragging minimap =
    let
        viewbox =
            Viewbox config.viewbox.width 60
    in
    div
        [ Html.Attributes.class (element "minimap" [])
        ]
        [ svg
            [ Svg.Attributes.viewBox (makeViewBox viewbox)
            , Svg.Attributes.class (element "svg" [])
            ]
            [ viewLines 1 settings (drawMinimap settings viewbox chart status minimap)
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
        [ Html.Attributes.id (nodeID settings.id "root")
        , Html.Attributes.class block
        ]
        [ viewCanvas settings chart state.status state.range state.canvs
        , viewContainer
            [ viewMinimap settings chart state.status state.range state.dragging state.minimap
            ]
        , viewContainer
            [ viewLinesVisibility state.status (Dict.values (Data.getChartLines chart))
            ]
        ]
