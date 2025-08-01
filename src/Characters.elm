module Characters exposing (main)

import Avataaars
import Avataaars.Clothes exposing (Clothes)
import Avataaars.Face exposing (Face)
import Avataaars.FacialHair as FacialHair
import Avataaars.Graphics exposing (Graphics(..))
import Avataaars.SkinTone exposing (SkinTone)
import Avataaars.Top as Top exposing (Top(..))
import Avatars
import Browser
import List.Extra
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (cursor, transform, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Events exposing (onClick)
import TypedSvg.Types exposing (Cursor(..), Transform(..))
import Types exposing (Character(..))


type alias Model =
    Maybe ( Character, Graphics )


type alias Msg =
    Model


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


main : Program () Model Msg
main =
    Browser.sandbox
        { init = Nothing
        , view = view
        , update = always
        }


view : Model -> Svg Msg
view model =
    let
        list =
            case model of
                Just focused ->
                    let
                        base : { circleBg : Bool, clothes : Clothes, skinTone : SkinTone, face : Face, top : Top }
                        base =
                            Avatars.characterToAvatar focused
                    in
                    (base.top :: allTops)
                        |> List.map (\top -> ( Nothing, { base | top = top } ))

                Nothing ->
                    allCharacters
                        |> List.map (\c -> ( Just c, Avatars.characterToAvatar c ))
    in
    list
        |> List.indexedMap
            (\i ( msg, config ) ->
                g
                    [ transform
                        [ Translate
                            (modBy 4 i |> toFloat)
                            ((i // 4) |> toFloat)
                        ]
                    , onClick msg
                    , cursor CursorPointer
                    ]
                    [ Avataaars.view
                        { width = 1
                        , height = 1
                        }
                        config
                    ]
            )
        |> svg
            [ viewBox -0.5 -0.5 5 ((List.length list + 3) // 4 + 1 |> toFloat)
            ]


allTops : List Top
allTops =
    [ topFacialHair
    , topHatColorAccessory
    , topAccessoryFacialHair
    , topHatColorAccessoryFacialHair
    , topHairColorAccessoryFacialHair
    ]
        |> List.concat


topFacialHair : List Top
topFacialHair =
    [ TopFacialHair Top.Eyepatch FacialHair.Blank ]


topHatColorAccessory : List Top
topHatColorAccessory =
    List.Extra.lift3
        (\t color accessory ->
            TopHatColorAccessory t color accessory
        )
        []
        []
        []


topAccessoryFacialHair : List Top
topAccessoryFacialHair =
    []


topHatColorAccessoryFacialHair : List Top
topHatColorAccessoryFacialHair =
    []


topHairColorAccessoryFacialHair : List Top
topHairColorAccessoryFacialHair =
    []
