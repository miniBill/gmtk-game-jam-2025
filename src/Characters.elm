module Characters exposing (main)

import Avataaars
import Avataaars.Graphics exposing (Graphics(..))
import Avatars
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (transform, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Transform(..))
import Types exposing (Character(..))


allCharacters : List ( Character, Graphics )
allCharacters =
    let
        go : ( Character, Graphics ) -> List ( Character, Graphics ) -> List ( Character, Graphics )
        go curr acc =
            case Types.next curr of
                ( June, _ ) ->
                    List.reverse (curr :: acc)

                next ->
                    go next (curr :: acc)
    in
    go ( June, Bat ) []


main : Svg msg
main =
    allCharacters
        |> List.indexedMap
            (\i c ->
                g [ transform [ Translate (modBy 4 i |> toFloat) ((i // 4) |> toFloat) ] ]
                    [ Avataaars.view
                        { width = 1
                        , height = 1
                        }
                        (Avatars.characterToAvatar c)
                    ]
            )
        |> svg
            [ viewBox -0.5 -0.5 5 8
            ]
