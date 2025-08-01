module Avatars exposing (characterToAvatar)

import Avataaars.Accessory as Accessory
import Avataaars.Clothes as Clothes exposing (Clothes)
import Avataaars.Eyebrow as Eyebrow exposing (Eyebrow)
import Avataaars.Eyes as Eyes exposing (Eyes)
import Avataaars.Face exposing (Face)
import Avataaars.FacialHair as FacialHair
import Avataaars.Graphics exposing (Graphics)
import Avataaars.HairColor as HairColor
import Avataaars.Mouth as Mouth exposing (Mouth)
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
    , clothes = Clothes.GraphicShirt (Types.characterToColor color) graphics
    , skinTone = characterToSkinTone color
    , face = characterToFace color
    , top = characterToTop color
    }


characterToSkinTone : Character -> SkinTone
characterToSkinTone character =
    case character of
        June ->
            "#ffffff"

        Rose ->
            "#ffffff"

        Dave ->
            "#ffffff"

        Jade ->
            "#ffffff"

        Aradia ->
            "#c4c4c4"

        Tavros ->
            "#c4c4c4"

        Sollux ->
            "#c4c4c4"

        Karkat ->
            "#c4c4c4"

        Nepeta ->
            "#c4c4c4"

        Kanaya ->
            "#c4c4c4"

        Terezi ->
            "#c4c4c4"

        Vriska ->
            "#c4c4c4"

        Equius ->
            "#c4c4c4"

        Gamzee ->
            "#c4c4c4"

        Eridan ->
            "#c4c4c4"

        Feferi ->
            "#c4c4c4"

        Jane ->
            "#ffffff"

        Dirk ->
            "#ffffff"

        Roxy ->
            "#ffffff"

        Jake ->
            "#ffffff"

        Calliope ->
            "#0b4100"

        Caliborn ->
            "#0b4100"


characterToFace : Character -> Face
characterToFace character =
    { mouth = characterToMouth character
    , eyebrow = characterToEyebrow character
    , eyes = characterToEyes character
    }


characterToMouth : Character -> Mouth
characterToMouth character =
    case character of
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

        Caliborn ->
            Mouth.Tongue

        _ ->
            Mouth.Default


characterToEyebrow : Character -> Eyebrow
characterToEyebrow character =
    case character of
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


characterToEyes : Character -> Eyes
characterToEyes character =
    case character of
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


characterToTop : Character -> Top
characterToTop character =
    let
        hairColorAccessoryFacialHair : Top.TopHairColorAccessoryFacialHair -> Top
        hairColorAccessoryFacialHair hair =
            Top.TopHairColorAccessoryFacialHair hair HairColor.black Accessory.Blank FacialHair.Blank
    in
    case character of
        June ->
            Top.TopHairColorAccessoryFacialHair Top.ShortHairShaggyMullet HairColor.black Accessory.Prescription01 FacialHair.Blank

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

        _ ->
            Top.TopHairColorAccessoryFacialHair Top.LongHairBigHair HairColor.black Accessory.Blank FacialHair.Blank
