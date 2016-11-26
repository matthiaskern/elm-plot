module Plot
    exposing
        ( base
        , baseStatic
        , xAxis
        , yAxis
        , verticalGrid
        , horizontalGrid
        , tooltip
        , area
        , line
        , Element
        , initialState
        , update
        , Interaction(..)
        , State
        )

{-|
 This library aims to allow you to visualize a variety of graphs in
 an intuitve manner without comprimising flexibility regarding configuration.
 It is insprired by the elm-html api, using the `element attrs children` pattern.

# Elements
@docs Element, base, baseStatic, line, area, xAxis, yAxis, tooltip, verticalGrid, horizontalGrid

# Configuration

# State
@docs State, initialState, update, Interaction


-}

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Svg exposing (g)
import Svg.Attributes exposing (height, width, d, style)
import Svg.Events exposing (onMouseOver)
import Svg.Lazy
import String
import Task
import Json.Decode as Json
import Dom
import Dom.Position
import Round
import Debug
import Helpers exposing (..)
import Element exposing (Element(..))

import Plot.Types exposing (..)
import Plot.Base as Base
import Plot.Axis as Axis
import Plot.Tick as Tick
import Plot.Grid as Grid
import Plot.Grid.Config as GridConfig
import Plot.Grid.View as GridView
import Plot.Area as Area
import Plot.Line as Line
import Plot.Tooltip as Tooltip


{-| Represents child element of the plot.
-}
type Element msg
    = Element (Element.Element msg)


{-| This returns an axis element resulting in an x-axis being rendered in your plot.

    main =
        plot [] [ xAxis [] ]
-}
xAxis : List (Axis.Attribute msg) -> Element msg
xAxis attrs =
    Element <| Axis (List.foldl (<|) Axis.defaultConfigX attrs)


{-| This returns an axis element resulting in an y-axis being rendered in your plot.

    main =
        plot [] [ yAxis [] ]
-}
yAxis : List (Axis.Attribute msg) -> Element msg
yAxis attrs =
    Element <| Axis (List.foldl (<|) Axis.defaultConfigY attrs)


{-| This returns an grid element resulting in vertical grid lines being rendered in your plot.

    main =
        plot [] [ horizontalGrid [] ]
-}
horizontalGrid : List Grid.Attribute -> Element msg
horizontalGrid attrs =
    Element <| Grid (foldConfig GridConfig.defaultConfigX attrs)


{-| This returns an axis element resulting in horizontal grid lines being rendered in your plot.

    main =
        plot [] [ verticalGrid [] ]
-}
verticalGrid : List Grid.Attribute -> Element msg
verticalGrid attrs =
    Element <| Grid (foldConfig GridConfig.defaultConfigY attrs)


{-| This returns an area element resulting in an area serie rendered in your plot.

    main =
        plot [] [ area []  [ ( 0, -2 ), ( 2, 0 ), ( 3, 1 ) ] ]
-}
area : List Area.Attribute -> List Point -> Element msg
area attrs points =
    Element <| Area (Area.toConfig attrs) points


{-| This returns a line element resulting in an line serie rendered in your plot.

    main =
        plot [] [ line [] [ ( 0, 1 ), ( 2, 2 ), ( 3, 4 ) ] ]
-}
line : List Line.Attribute -> List Point -> Element msg
line attrs points =
    Element <| Line (List.foldr (<|) Line.defaultConfig attrs) points


{-|
-}
tooltip : List (Tooltip.Attribute msg) -> Maybe Point -> Element msg
tooltip attrs position =
    Element <| Tooltip (List.foldr (<|) Tooltip.defaultConfig attrs) position


{-| This is the function processing your entire plot configuration.
 Pass your meta attributes and plot elements to this function and
 a svg plot will be returned!
-}
base : List Base.Attribute -> List (Element (Interaction c)) -> Svg.Svg (Interaction c)
base attrs elements =
    Svg.Lazy.lazy2 parsePlot attrs elements


{-| -}
baseStatic : List Base.Attribute -> List (Element msg) -> Svg.Svg msg
baseStatic attrs elements =
    Svg.Lazy.lazy2 parsePlotStatic attrs elements


-- MODEL


{-| -}
type alias State =
    { position : Maybe ( Float, Float )
    , waiting : Bool
    }


{-| -}
initialState : State
initialState =
    { position = Nothing
    , waiting = True
    }



-- UPDATE


{-| -}
type Interaction c
    = Internal Msg
    | Custom c


type Msg
    = Hovering Meta ( Float, Float )
    | ReceivePosition (Result Dom.Error Point)
    | ResetPosition


{-| -}
update : Msg -> State -> ( State, Cmd (Interaction c) )
update msg state =
    case msg of
        Hovering meta eventPosition ->
            ( { state | waiting = True }, getPosition meta eventPosition )

        ReceivePosition result ->
            case result of
                Ok position ->
                    if state.waiting then
                        ( { state | position = Just position }, Cmd.none )
                    else
                        ( state, Cmd.none )

                Err err ->
                    ( state, Cmd.none )

        ResetPosition ->
            ( { position = Nothing, waiting = False }, Cmd.none )



getPosition : Meta -> ( Float, Float ) -> Cmd (Interaction c)
getPosition meta eventPosition =
    Task.map2
        (getRelativePosition meta eventPosition)
        (Dom.Position.left meta.id)
        (Dom.Position.top meta.id)
        |> Task.attempt ReceivePosition
        |> Cmd.map Internal


getRelativePosition : Meta -> ( Float, Float ) -> Float -> Float -> Point
getRelativePosition { fromSvgCoords, toNearestX } ( mouseX, mouseY ) left top =
    let
        ( x, y ) =
            fromSvgCoords ( mouseX - left, mouseY - top )
    in
        ( toNearestX x, y )



-- VIEW


parsePlotStatic : List Base.Attribute -> List (Element msg) -> Svg.Svg msg
parsePlotStatic attrs elements =
    let
        metaConfig =
            Base.toConfig attrs

        meta =
            calculateMeta metaConfig elements
    in
        viewPlotStatic metaConfig meta (viewElements meta elements)


parsePlot : List Base.Attribute -> List (Element (Interaction c)) -> Svg.Svg (Interaction c)
parsePlot attrs elements =
    let
        metaConfig =
            Base.toConfig attrs

        meta =
            calculateMeta metaConfig elements
    in
        viewPlot metaConfig meta (viewElements meta elements)


getMousePosition : Meta -> Json.Decoder (Interaction c)
getMousePosition meta =
    Json.map2
        (\x y -> Internal <| Hovering meta ( x, y ))
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)


viewPlot : Base.Config -> Meta -> ( List (Svg.Svg (Interaction c)), List (Html.Html (Interaction c)) ) -> Svg.Svg (Interaction c)
viewPlot { size, style, classes, margin } meta ( svgViews, htmlViews ) =
    let
        ( width, height ) =
            size

        ( top, right, bottom, left ) =
            margin

        sizeStyle =
            [ ( "height", toString height ++ "px" ), ( "width", toString width ++ "px" ) ]
    in
        Html.div
            [ Html.Attributes.class "elm-plot"
            , Html.Attributes.style sizeStyle
            , Html.Attributes.id meta.id
            , Html.Events.on "mousemove" (getMousePosition meta)
            , Html.Events.onMouseOut (Internal ResetPosition)
            ]
        <|
            [ Svg.svg
                [ Svg.Attributes.height (toString height)
                , Svg.Attributes.width (toString width)
                , Svg.Attributes.viewBox <| "0 0 " ++ toString width ++ " " ++ toString height
                , Svg.Attributes.class "elm-plot__svg"
                ]
                svgViews
            ]
                ++ htmlViews



viewPlotStatic : Base.Config -> Meta -> ( List (Svg.Svg msg), List (Html.Html msg) ) -> Svg.Svg msg
viewPlotStatic { size, style, classes, margin } meta ( svgViews, htmlViews ) =
    let
        ( width, height ) =
            size

        ( top, right, bottom, left ) =
            margin

        sizeStyle =
            [ ( "height", toString height ++ "px" ), ( "width", toString width ++ "px" ) ]
    in
        Html.div
            [ Html.Attributes.class "elm-plot"
            , Html.Attributes.style sizeStyle
            , Html.Attributes.id meta.id
            ]
        <|
            [ Svg.svg
                [ Svg.Attributes.height (toString height)
                , Svg.Attributes.width (toString width)
                , Svg.Attributes.viewBox <| "0 0 " ++ toString width ++ " " ++ toString height
                , Svg.Attributes.class "elm-plot__svg"
                ]
                svgViews
            ]
                ++ htmlViews


-- VIEW ELEMENTS


viewElements : Meta -> List (Element msg) -> ( List (Svg.Svg msg), List (Html.Html msg) )
viewElements meta elements =
    List.foldr (viewElement meta) ( [], [] ) elements


viewElement : Meta -> Element msg -> ( List (Svg.Svg msg), List (Html.Html msg) ) -> ( List (Svg.Svg msg), List (Html.Html msg) )
viewElement meta (Element element) ( svgViews, htmlViews ) =
    case element of
        Line config points ->
            ( (Line.view meta config points) :: svgViews, htmlViews )

        Area config points ->
            ( (Area.view meta config points) :: svgViews, htmlViews )

        Axis ({ orientation } as config) ->
            ( (Axis.defaultView (getFlippedMeta orientation meta) config) :: svgViews, htmlViews )

        Grid ({ orientation } as config) ->
            ( (GridView.view (getFlippedMeta orientation meta) config) :: svgViews, htmlViews )

        Tooltip config position ->
            case position of
                Just point ->
                    ( svgViews, (Tooltip.view meta config point) :: htmlViews )

                Nothing ->
                    ( svgViews, htmlViews )



-- CALCULATIONS


calculateMeta : Base.Config -> List (Element msg) -> Meta
calculateMeta { size, padding, margin, id } elements =
    let
        ( xValues, yValues ) =
            List.unzip (List.foldr collectPoints [] elements)

        ( width, height ) =
            size

        ( top, right, bottom, left ) =
            margin

        xScale =
            getScale width ( left, right ) ( 0, 0 ) xValues

        yScale =
            getScale height ( top, bottom ) padding yValues

        xTicks =
            getLastGetTickValues X elements <| xScale

        yTicks =
            getLastGetTickValues Y elements <| yScale
    in
        { scale = xScale
        , oppositeScale = yScale
        , oppositeToSvgCoords = toSvgCoordsY xScale yScale
        , toSvgCoords = toSvgCoordsX xScale yScale
        , fromSvgCoords = fromSvgCoords xScale yScale
        , ticks = xTicks
        , oppositeTicks = yTicks
        , toNearestX = toNearestX xValues
        , getTooltipInfo = getTooltipInfo elements
        , id = id
        }


flipToY : Meta -> Meta
flipToY ({ scale, oppositeScale, toSvgCoords, oppositeToSvgCoords, ticks, oppositeTicks } as meta) =
    { meta
        | scale = oppositeScale
        , oppositeScale = scale
        , toSvgCoords = oppositeToSvgCoords
        , oppositeToSvgCoords = toSvgCoords
        , ticks = oppositeTicks
        , oppositeTicks = ticks
    }


getFlippedMeta : Orientation -> Meta -> Meta
getFlippedMeta orientation meta =
    case orientation of
        X ->
            meta

        Y ->
            flipToY meta


getScale : Float -> ( Float, Float ) -> ( Float, Float ) -> List Float -> Scale
getScale lengthTotal ( offsetLeft, offsetRight ) ( paddingBottomPx, paddingTopPx ) values =
    let
        length =
            lengthTotal - offsetLeft - offsetRight

        lowest =
            getLowest values

        highest =
            getHighest values

        range =
            getRange lowest highest

        paddingTop =
            pixelsToValue length range paddingTopPx

        paddingBottom =
            pixelsToValue length range paddingBottomPx
    in
        { lowest = lowest - paddingBottom
        , highest = highest + paddingTop
        , range = range + paddingBottom + paddingTop
        , length = length
        , offset = offsetLeft
        }


scaleValue : Scale -> Float -> Float
scaleValue { length, range, offset } v =
    (v * length / range) + offset


unScaleValue : Scale -> Float -> Float
unScaleValue { length, range, offset } v =
    (v - offset) * range / length


fromSvgCoords : Scale -> Scale -> Point -> Point
fromSvgCoords xScale yScale ( x, y ) =
    ( unScaleValue xScale x
    , unScaleValue yScale (yScale.length - y)
    )


toSvgCoordsX : Scale -> Scale -> Point -> Point
toSvgCoordsX xScale yScale ( x, y ) =
    ( scaleValue xScale (abs xScale.lowest + x)
    , scaleValue yScale (yScale.highest - y)
    )


toSvgCoordsY : Scale -> Scale -> Point -> Point
toSvgCoordsY xScale yScale ( x, y ) =
    toSvgCoordsX xScale yScale ( y, x )


getDifference : Float -> Float -> Float
getDifference a b =
    abs <| (abs a) - (abs b)


getClosest : Float -> Float -> Float -> Float
getClosest value candidate closest =
    if getDifference value candidate < getDifference value closest then
        candidate
    else
        closest


toNearestX : List Float -> Float -> Float
toNearestX xValues value =
    List.foldr (getClosest value) 0 xValues


getTooltipInfo : List (Element msg) -> Float -> TooltipInfo
getTooltipInfo elements xValue =
    TooltipInfo xValue <| List.foldr (collectYValues xValue) [] elements


getAxisConfig : Orientation -> Element msg -> Maybe (Axis.Config msg) -> Maybe (Axis.Config msg)
getAxisConfig orientation (Element element) lastConfig =
    case element of
        Axis config ->
            if config.orientation == orientation then
                Just config
            else
                lastConfig

        _ ->
            lastConfig


getLastGetTickValues : Orientation -> List (Element msg) -> Scale -> List Float
getLastGetTickValues orientation elements =
    List.foldl (getAxisConfig orientation) Nothing elements
        |> Maybe.withDefault Axis.defaultConfigX
        |> .tickConfig
        |> Tick.getValues


collectPoints : Element msg -> List Point -> List Point
collectPoints (Element element) allPoints =
    case element of
        Area config points ->
            allPoints ++ points

        Line config points ->
            allPoints ++ points

        _ ->
            allPoints


collectYValues : Float -> Element msg -> List (Maybe Float) -> List (Maybe Float)
collectYValues xValue (Element element) yValues =
    case element of
        Area config points ->
            collectYValue xValue points :: yValues

        Line config points ->
            collectYValue xValue points :: yValues

        _ ->
            yValues


collectYValue : Float -> List Point -> Maybe Float
collectYValue xValue points =
    List.foldr (getYValue xValue) Nothing points


getYValue : Float -> Point -> Maybe Float -> Maybe Float
getYValue xValue ( x, y ) result =
    if x == xValue then
        Just y
    else
        result
