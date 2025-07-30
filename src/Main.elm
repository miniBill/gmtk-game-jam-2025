module Main exposing (Model, Msg, main)

import Avataaars
import Avataaars.Accessory as Accessory
import Avataaars.Clothes as Clothes
import Avataaars.Eyebrow as Eyebrow
import Avataaars.Eyes as Eyes
import Avataaars.Face exposing (Face)
import Avataaars.FacialHair as FacialHair
import Avataaars.Graphics exposing (Graphics(..))
import Avataaars.HairColor as HairColor
import Avataaars.Mouth as Mouth
import Avataaars.Top as Top exposing (Top)
import Browser
import Svg exposing (Svg, g, svg)
import Svg.Attributes exposing (cursor, transform, viewBox)
import Svg.Events exposing (onClick)
import Types exposing (Color(..), Flags)


type alias Model =
    { currentAvatar : ( Color, Graphics )
    }


type Msg
    = Next
    | Previous


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : flags -> ( Model, Cmd msg )
init _ =
    ( { currentAvatar = ( Aradia, Bat ) }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Next ->
            ( { currentAvatar = next model.currentAvatar
              }
            , Cmd.none
            )

        Previous ->
            ( { currentAvatar = previous model.currentAvatar
              }
            , Cmd.none
            )


next : ( Color, Graphics ) -> ( Color, Graphics )
next ( color, graphics ) =
    ( Types.nextColor color
    , Types.nextGraphics graphics
    )


previous : ( Color, Graphics ) -> ( Color, Graphics )
previous ( color, graphics ) =
    ( Types.previousColor color
    , Types.previousGraphics graphics
    )


view : Model -> Svg Msg
view model =
    List.range 0 11
        |> List.map
            (\i ->
                let
                    ( color, graphics ) =
                        iterate i next model.currentAvatar
                in
                g
                    [ transform
                        ("translate("
                            ++ String.fromInt (modBy 4 i * 50)
                            ++ " "
                            ++ String.fromInt (i // 4 * 50)
                            ++ ")"
                        )
                    , cursor "pointer"
                    , onClick
                        (if i < 6 then
                            Previous

                         else
                            Next
                        )
                    ]
                    [ Avataaars.view
                        { width = 50
                        , height = 50
                        }
                        { circleBg = True
                        , clothes = Clothes.GraphicShirt (Types.colorToString color) graphics
                        , skinTone = "#c4c4c4"
                        , face = colorToFace color
                        , top = colorToTop color
                        }
                    ]
            )
        |> svg [ viewBox "-10 -10 220 170" ]


iterate : Int -> (a -> a) -> a -> a
iterate i f x =
    if i <= 0 then
        x

    else
        iterate (i - 1) f (f x)


colorToTop : Color -> Top
colorToTop color =
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


colorToFace : Color -> Face
colorToFace color =
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


subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none
