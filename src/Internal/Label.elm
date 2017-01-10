module Internal.Label
    exposing
        ( ViewConfig(..)
        , StyleConfig
        , FormatConfig(..)
        , defaultViewConfig
        , defaultStyleConfig
        , view
        , defaultView
        )

import Internal.Types exposing (Point, Style, Orientation(..), Scale, Meta, HintInfo, Value, IndexedInfo)
import Internal.Draw as Draw exposing (..)
import Svg
import Svg.Attributes


type alias StyleConfig msg =
    { displace : Maybe ( Int, Int )
    , style : Style
    , classes : List String
    , customAttrs : List (Svg.Attribute msg)
    }


type FormatConfig a
    = FromFunc (a -> String)
    | FromList (List String)


type ViewConfig a msg
    = FromStyle (StyleConfig msg)
    | FromStyleDynamic (a -> StyleConfig msg)
    | FromCustomView (a -> Svg.Svg msg)


defaultViewConfig : ViewConfig a msg
defaultViewConfig =
    FromStyle defaultStyleConfig


defaultStyleConfig : StyleConfig msg
defaultStyleConfig =
    { displace = Nothing
    , style = []
    , classes = []
    , customAttrs = []
    }


view : ViewConfig a msg -> FormatConfig a -> (a -> List (Svg.Attribute msg)) -> List a -> List (Svg.Svg msg)
view viewConfig formatConfig toAttributes infos =
    case viewConfig of
        FromStyle styles ->
            viewLabels formatConfig (\info text -> Svg.g (toAttributes info) [ defaultView styles text ]) infos

        FromStyleDynamic toStyleAttributes ->
            viewLabels formatConfig (\info text -> Svg.g (toAttributes info) [ defaultView (toStyleAttributes info) text ]) infos

        FromCustomView view ->
            List.map (\info -> Svg.g (toAttributes info) [ view info ]) infos


viewLabels : FormatConfig a -> (a -> String -> Svg.Svg msg) -> List a -> List (Svg.Svg msg)
viewLabels formatConfig view infos =
    case formatConfig of
        FromFunc formatter ->
            List.map (\info -> view info (formatter info)) infos

        FromList texts ->
            List.map2 view infos texts


defaultView : StyleConfig msg -> String -> Svg.Svg msg
defaultView { displace, style, classes, customAttrs } text =
    let
        ( dx, dy ) =
            Maybe.withDefault ( 0, 0 ) displace

        attrs =
            [ Svg.Attributes.transform (toTranslate ( toFloat dx, toFloat dy ))
            , Svg.Attributes.style (toStyle style)
            , Svg.Attributes.class (String.join " " ("elm-plot__label__default-view" :: classes))
            ]
                ++ customAttrs
    in
        Svg.text_ attrs [ Svg.tspan [] [ Svg.text text ] ]
