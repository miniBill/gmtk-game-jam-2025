module Svg.Attributes.Extra exposing (transformTranslate, translate)

import Svg exposing (Attribute)
import Svg.Attributes


transformTranslate : Float -> Float -> Attribute msg
transformTranslate x y =
    Svg.Attributes.transform (translate x y)


translate : Float -> Float -> String
translate x y =
    "translate(" ++ String.fromFloat x ++ " " ++ String.fromFloat y ++ ")"
