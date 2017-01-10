module Plot.Label
    exposing
        ( View
        , FormatConfig
        , StyleAttribute
        , view
        , viewCustom
        , stroke
        , strokeWidth
        , opacity
        , fill
        , fontSize
        , classes
        , customAttrs
        , displace
        , format
        , formatFromList
        )

{-|
 Attributes for altering the view of your labels.

 Before you read any further, please note that when I speak of the label _index_,
 then I'm talking about how many labels that particular label is from the origin.

 Ok, now you can go on!

# Definition
@docs View, FormatConfig

# View options
@docs view, viewCustom

## Style attributes
If these attributes do not forfill your needs, try out the viewCustom! If you have
a suspicion that I have missed a very common configuration, then please let me know and I'll add it.
@docs StyleAttribute, classes, displace, stroke, strokeWidth, opacity, fill, fontSize, customAttrs

# Value formatting
@docs format, formatFromList

-}

import Svg
import Internal.Label as Internal
import Internal.Draw exposing (..)


{-| -}
type alias View a msg =
    Internal.View a msg


{-| -}
type alias FormatConfig a =
    Internal.FormatConfig a


{-| -}
type alias StyleAttribute msg =
    Internal.StyleConfig msg -> Internal.StyleConfig msg


{-| Displaces the label.

    myYAxis : Plot.Element msg
    myYAxis =
        Plot.yAxis
            [ Axis.label
                [ Label.view
                    [ Label.displace ( 12, 0 ) ]
                ]
            ]
-}
displace : ( Int, Int ) -> StyleAttribute msg
displace displace config =
    { config | displace = Just displace }


{-| Adds classes to the label.

    myYAxis : Plot.Element msg
    myYAxis =
        Plot.yAxis
            [ Axis.label
                [ Label.view
                    [ Label.classes [ "my-class" ] ]
                ]
            ]
-}
classes : List String -> StyleAttribute msg
classes classes config =
    { config | classes = classes }


{-| Set the stroke color.
-}
stroke : String -> StyleAttribute msg
stroke stroke config =
    { config | style = ( "stroke", stroke ) :: config.style }


{-| Set the stroke width (in pixels).
-}
strokeWidth : Int -> StyleAttribute msg
strokeWidth strokeWidth config =
    { config | style = ( "stroke-width", toPixelsInt strokeWidth ) :: config.style }


{-| Set the fill color.
-}
fill : String -> StyleAttribute msg
fill fill config =
    { config | style = ( "fill", fill ) :: config.style }


{-| Set the opacity.
-}
opacity : Float -> StyleAttribute msg
opacity opacity config =
    { config | style = ( "opacity", toString opacity ) :: config.style }


{-| Set the font size (in pixels).
-}
fontSize : Int -> StyleAttribute msg
fontSize fontSize config =
    { config | style = ( "font-size", toPixelsInt fontSize ) :: config.style }


{-| Add your own attributes. For events, see [this example](https://github.com/terezka/elm-plot/blob/master/examples/Interactive.elm)
-}
customAttrs : List (Svg.Attribute msg) -> StyleAttribute msg
customAttrs attrs config =
    { config | customAttrs = attrs }


{-| Provide a list of style attributes to alter the view of the label.

    myYAxis : Plot.Element msg
    myYAxis =
        Plot.yAxis
            [ Axis.label
                [ Label.view
                    [ Label.classes [ "label-class" ]
                    , Label.displace ( 12, 0 )
                    ]
                ]
            ]

 **Note:** If you add another attribute altering the view like `viewDynamic` or `viewCustom` _after_ this attribute,
 then this attribute will have no effect.
-}
view : (a -> List (StyleAttribute msg)) -> View a msg
view toStyles =
    (\info text -> Internal.defaultView (Internal.toStyleConfig (toStyles info)) info text)


{-| Define your own view for the labels. Your view will be passed label's value and index (amount of ticks from origin).

    viewLabel : Int -> Float -> Svg.Svg a
    viewLabel index value =
        let
            attrs =
                if isOdd index then oddAttrs else evenAttrs
        in
            text_ attrs (toString value)

    myYAxis : Plot.Element msg
    myYAxis =
        Plot.yAxis
            [ Axis.label
                [ Label.viewCustom viewLabel ]
            ]

 **Note:** If you add another attribute altering the view like `view` or `viewDynamic` _after_ this attribute,
 then this attribute will have no effect.
-}
viewCustom : (a -> String -> Svg.Svg msg) -> View a msg
viewCustom toView =
    toView


{-| Format the label based on its value and index.

    formatter : Int -> Float -> String
    formatter index value =
        if isDivisibleBy5 index then
            formatEveryFifth value
        else
            normalFormat value

    myYAxis : Plot.Element msg
    myYAxis =
        Plot.yAxis
            [ Axis.label
                [ Label.format formatter ]
            ]
-}
format : (a -> String) -> FormatConfig a
format format =
    Internal.FromFunc format


{-| -}
formatFromList : List String -> FormatConfig a
formatFromList format =
    Internal.FromList format
