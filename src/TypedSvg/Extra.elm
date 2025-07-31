module TypedSvg.Extra exposing (centeredText)

import TypedSvg exposing (text_)
import TypedSvg.Attributes exposing (dominantBaseline, textAnchor)
import TypedSvg.Core exposing (Attribute, Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), DominantBaseline(..))


centeredText : List (Attribute msg) -> List (Svg msg) -> Svg msg
centeredText attrs children =
    text_
        (textAnchor AnchorMiddle
            :: dominantBaseline DominantBaselineCentral
            :: attrs
        )
        children
