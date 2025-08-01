module Characters exposing (main)

import Avataaars
import Avataaars.Accessory as Accessory exposing (Accessory)
import Avataaars.Clothes exposing (Clothes)
import Avataaars.Face exposing (Face)
import Avataaars.FacialHair as FacialHair exposing (FacialHair)
import Avataaars.Graphics exposing (Graphics(..))
import Avataaars.HairColor as HairColor exposing (HairColor)
import Avataaars.HatColor as HatColor exposing (HatColor)
import Avataaars.SkinTone exposing (SkinTone)
import Avataaars.Top as Top exposing (Top(..))
import Avatars
import Browser
import TypedSvg exposing (g, svg, title)
import TypedSvg.Attributes exposing (cursor, transform, viewBox)
import TypedSvg.Core exposing (Svg, text)
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
        perRow : number
        perRow =
            8

        list :
            List
                ( Maybe ( Character, Graphics )
                , String
                , { circleBg : Bool
                  , clothes : Clothes
                  , skinTone : SkinTone
                  , face : Face
                  , top : Top
                  }
                )
        list =
            case model of
                Just focused ->
                    let
                        base : { circleBg : Bool, clothes : Clothes, skinTone : SkinTone, face : Face, top : Top }
                        base =
                            Avatars.characterToAvatar focused
                    in
                    (base.top :: allTops base.top)
                        |> List.map
                            (\top ->
                                ( Nothing
                                , Debug.toString top
                                , { base | top = top }
                                )
                            )

                Nothing ->
                    allCharacters
                        |> List.map
                            (\c ->
                                ( Just c
                                , Debug.toString (Tuple.first c)
                                , Avatars.characterToAvatar c
                                )
                            )
    in
    list
        |> List.indexedMap
            (\i ( msg, label, config ) ->
                g
                    [ transform
                        [ Translate
                            (modBy perRow i |> toFloat)
                            ((i // perRow) |> toFloat)
                        ]
                    , onClick msg
                    , cursor CursorPointer
                    ]
                    [ Avataaars.view
                        { width = 1
                        , height = 1
                        }
                        config
                    , title [] [ text label ]
                    ]
            )
        |> svg
            [ viewBox
                -0.1
                -0.1
                (perRow + 0.2)
                (1.2 + toFloat ((List.length list + 3) // perRow))
            ]


allTops : Top -> List Top
allTops top =
    [ topFacialHair top
    , topHatColorAccessory top
    , topAccessoryFacialHair top
    , topHatColorAccessoryFacialHair top
    , topHairColorAccessoryFacialHair top
    ]
        |> List.concat


topFacialHair : Top -> List Top
topFacialHair top =
    [ TopFacialHair Top.Eyepatch (topToFacialHair top) ]


topHatColorAccessory : Top -> List Top
topHatColorAccessory top =
    [ TopHatColorAccessory Top.Hijab (topToHatColor top) (topToAccessory top) ]


topAccessoryFacialHair : Top -> List Top
topAccessoryFacialHair top =
    List.map (\f -> TopAccessoryFacialHair f (topToAccessory top) (topToFacialHair top))
        [ Top.NoHair
        , Top.Hat
        , Top.LongHairFrida
        , Top.LongHairShavedSides
        ]


topHatColorAccessoryFacialHair : Top -> List Top
topHatColorAccessoryFacialHair top =
    List.map (\f -> TopHatColorAccessoryFacialHair f (topToHatColor top) (topToAccessory top) (topToFacialHair top))
        [ Top.Turban
        , Top.WinterHat1
        , Top.WinterHat2
        , Top.WinterHat3
        , Top.WinterHat4
        ]


topHairColorAccessoryFacialHair : Top -> List Top
topHairColorAccessoryFacialHair top =
    List.map (\f -> TopHairColorAccessoryFacialHair f (topToHairColor top) (topToAccessory top) (topToFacialHair top))
        [ Top.LongHairBigHair
        , Top.LongHairBob
        , Top.LongHairBun
        , Top.LongHairCurly
        , Top.LongHairCurvy
        , Top.LongHairDreads
        , Top.LongHairFro
        , Top.LongHairFroBand
        , Top.LongHairNotTooLong
        , Top.LongHairMiaWallace
        , Top.LongHairStraight
        , Top.LongHairStraight2
        , Top.LongHairStraightStrand
        , Top.ShortHairDreads01
        , Top.ShortHairDreads02
        , Top.ShortHairFrizzle
        , Top.ShortHairShaggyMullet
        , Top.ShortHairShortCurly
        , Top.ShortHairShortFlat
        , Top.ShortHairShortRound
        , Top.ShortHairShortWaved
        , Top.ShortHairSides
        , Top.ShortHairTheCaesar
        , Top.ShortHairTheCaesarSidePart
        ]


topToFacialHair : Top -> FacialHair
topToFacialHair top =
    case top of
        TopFacialHair _ r ->
            r

        TopHatColorAccessory _ _ _ ->
            FacialHair.Blank

        TopAccessoryFacialHair _ _ r ->
            r

        TopHatColorAccessoryFacialHair _ _ _ r ->
            r

        TopHairColorAccessoryFacialHair _ _ _ r ->
            r


topToHatColor : Top -> HatColor
topToHatColor top =
    case top of
        TopFacialHair _ _ ->
            HatColor.black

        TopHatColorAccessory _ r _ ->
            r

        TopAccessoryFacialHair _ _ _ ->
            HatColor.black

        TopHatColorAccessoryFacialHair _ r _ _ ->
            r

        TopHairColorAccessoryFacialHair _ r _ _ ->
            r


topToAccessory : Top -> Accessory
topToAccessory top =
    case top of
        TopFacialHair _ _ ->
            Accessory.Blank

        TopHatColorAccessory _ _ r ->
            r

        TopAccessoryFacialHair _ r _ ->
            r

        TopHatColorAccessoryFacialHair _ _ r _ ->
            r

        TopHairColorAccessoryFacialHair _ _ r _ ->
            r


topToHairColor : Top -> HairColor
topToHairColor top =
    case top of
        TopFacialHair _ _ ->
            HairColor.black

        TopHatColorAccessory _ _ _ ->
            HairColor.black

        TopAccessoryFacialHair _ _ _ ->
            HairColor.black

        TopHatColorAccessoryFacialHair _ _ _ _ ->
            HairColor.black

        TopHairColorAccessoryFacialHair _ r _ _ ->
            r
