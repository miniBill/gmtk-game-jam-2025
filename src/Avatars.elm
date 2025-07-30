module Avatars exposing (characterToAvatar)

import Avataaars.Accessory as Accessory
import Avataaars.Clothes as Clothes exposing (Clothes)
import Avataaars.Eyebrow as Eyebrow
import Avataaars.Eyes as Eyes
import Avataaars.Face exposing (Face)
import Avataaars.FacialHair as FacialHair
import Avataaars.Graphics exposing (Graphics)
import Avataaars.HairColor as HairColor
import Avataaars.Mouth as Mouth
import Avataaars.SkinTone exposing (SkinTone)
import Avataaars.Top as Top exposing (Top)
import Types exposing (Character(..))


characterToAvatar :
    ( Character, Graphics )
    ->
        { circleBg : Bool
        , clothes : Clothes
        , skinTone : SkinTone
        , face : Face
        , top : Top
        }
characterToAvatar ( color, graphics ) =
    { circleBg = True
    , clothes = Clothes.GraphicShirt (Types.colorToString color) graphics
    , skinTone = "#c4c4c4"
    , face = characterToFace color
    , top = characterToTop color
    }


characterToFace : Character -> Face
characterToFace color =
    { mouth =
        case color of
            Aradia ->
                Mouth.Smile

            Sollux ->
                Mouth.Serious

            Karkat ->
                Mouth.ScreamOpen

            Nepeta ->
                Mouth.Eating

            Kanaya ->
                Mouth.Twinkle

            Equius ->
                Mouth.Grimace

            Vriska ->
                Mouth.Smile

            Gamzee ->
                Mouth.Tongue

            _ ->
                Mouth.Default
    , eyebrow =
        case color of
            Sollux ->
                Eyebrow.Angry

            Karkat ->
                Eyebrow.Angry

            Vriska ->
                Eyebrow.Angry

            Gamzee ->
                Eyebrow.UnibrowNatural

            Eridan ->
                Eyebrow.UpDown

            _ ->
                Eyebrow.Default
    , eyes =
        case color of
            Aradia ->
                Eyes.Dizzy

            Sollux ->
                Eyes.EyeRoll

            Nepeta ->
                Eyes.Hearts

            Kanaya ->
                Eyes.Side

            Terezi ->
                Eyes.Close

            Vriska ->
                Eyes.Squint

            Gamzee ->
                Eyes.Surprised

            Eridan ->
                Eyes.Squint

            _ ->
                Eyes.Default
    }


characterToTop : Character -> Top
characterToTop color =
    let
        hairColorAccessoryFacialHair : Top.TopHairColorAccessoryFacialHair -> Top
        hairColorAccessoryFacialHair hair =
            Top.TopHairColorAccessoryFacialHair hair HairColor.black Accessory.Blank FacialHair.Blank
    in
    case color of
        Aradia ->
            hairColorAccessoryFacialHair Top.LongHairCurvy

        Tavros ->
            hairColorAccessoryFacialHair Top.ShortHairFrizzle

        Sollux ->
            Top.TopHairColorAccessoryFacialHair Top.ShortHairShortFlat HairColor.black Accessory.Wayfarers FacialHair.Blank

        Karkat ->
            hairColorAccessoryFacialHair Top.ShortHairDreads02

        Nepeta ->
            Top.TopHatColorAccessoryFacialHair Top.WinterHat4 "#003bff" Accessory.Blank FacialHair.Blank

        Kanaya ->
            hairColorAccessoryFacialHair Top.ShortHairShaggyMullet

        Terezi ->
            Top.TopHairColorAccessoryFacialHair Top.LongHairMiaWallace HairColor.black Accessory.Sunglasses FacialHair.Blank

        Vriska ->
            hairColorAccessoryFacialHair Top.LongHairStraight2

        Equius ->
            Top.TopHairColorAccessoryFacialHair Top.ShortHairTheCaesarSidePart HairColor.black Accessory.Sunglasses FacialHair.Blank

        Gamzee ->
            hairColorAccessoryFacialHair Top.LongHairDreads

        Eridan ->
            hairColorAccessoryFacialHair Top.ShortHairShortCurly

        Feferi ->
            Top.TopHairColorAccessoryFacialHair Top.LongHairBigHair HairColor.black Accessory.Kurt FacialHair.Blank
