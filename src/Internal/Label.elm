module Internal.Label
    exposing
        ( StyleConfig
        , FormatConfig(..)
        , View
        , ViewWrap
        , defaultViewConfig
        , defaultStyleConfig
        , toStyleConfig
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


type alias View a msg =
    a -> String -> Svg.Svg msg


type alias ViewWrap a msg =
    a -> Svg.Svg msg -> Svg.Svg msg


defaultViewConfig : View a msg
defaultViewConfig =
    defaultView defaultStyleConfig


defaultStyleConfig : StyleConfig msg
defaultStyleConfig =
    { displace = Nothing
    , style = []
    , classes = []
    , customAttrs = []
    }


toStyleConfig : List (StyleConfig msg -> StyleConfig msg) -> StyleConfig msg
toStyleConfig =
    List.foldl (<|) defaultStyleConfig


view : FormatConfig a -> View a msg -> ViewWrap a msg -> List a -> List (Svg.Svg msg)
view formatConfig view wrapView infos =
    toLabelText formatConfig infos
        |> List.map2 view infos
        |> List.map2 wrapView infos


toLabelText : FormatConfig a -> List a -> List String
toLabelText formatConfig infos =
    case formatConfig of
        FromFunc formatter ->
            List.map formatter infos

        FromList texts ->
            texts


defaultView : StyleConfig msg -> View a msg
defaultView { displace, style, classes, customAttrs } _ text =
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
