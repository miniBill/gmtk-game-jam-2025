module Color.Extra exposing (colorFromHex)

import Color exposing (Color)
import Hex


colorFromHex : String -> Color
colorFromHex hex =
    let
        r : String
        r =
            String.slice 0 2 hex

        g : String
        g =
            String.slice 2 4 hex

        b : String
        b =
            String.slice 4 6 hex

        i : String -> Int
        i v =
            v
                |> Hex.fromString
                |> Result.withDefault 0
    in
    Color.rgb255 (i r) (i g) (i b)
