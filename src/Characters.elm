module Characters exposing (main)

import Avataaars
import Avataaars.Accessory as Accessory exposing (Accessory)
import Avataaars.Clothes exposing (Clothes)
import Avataaars.Eyebrow as Eyebrow exposing (Eyebrow)
import Avataaars.Eyes as Eyes exposing (Eyes)
import Avataaars.Face exposing (Face)
import Avataaars.FacialHair as FacialHair exposing (FacialHair)
import Avataaars.Graphics exposing (Graphics(..))
import Avataaars.HairColor as HairColor exposing (HairColor)
import Avataaars.HatColor as HatColor exposing (HatColor)
import Avataaars.Mouth as Mouth exposing (Mouth)
import Avataaars.SkinTone exposing (SkinTone)
import Avataaars.Top as Top exposing (Top(..))
import Avatars
import Browser
import List.Extra
import TypedSvg exposing (g, svg, tspan)
import TypedSvg.Attributes exposing (cursor, transform, viewBox)
import TypedSvg.Attributes.InEm
import TypedSvg.Attributes.InPx exposing (fontSize, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Events exposing (onClick)
import TypedSvg.Extra exposing (centeredText)
import TypedSvg.Types exposing (Cursor(..), Transform(..))
import Types exposing (Character(..))


type alias Config =
    { circleBg : Bool
    , clothes : Clothes
    , skinTone : SkinTone
    , face : Face
    , top : Top
    }


type alias Model =
    Maybe ( Character, Graphics, Config )


type alias Msg =
    Model


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
        perRow : Int
        perRow =
            list
                |> List.map List.length
                |> List.maximum
                |> Maybe.withDefault 1

        list : List (List ( Msg, Config ))
        list =
            case model of
                Just selected ->
                    allVariants selected

                Nothing ->
                    Types.allCharacters
                        |> List.map
                            (\( character, graphics ) ->
                                let
                                    config : Config
                                    config =
                                        Avatars.characterToAvatar ( character, graphics )
                                in
                                ( Just ( character, graphics, config )
                                , config
                                )
                            )
                        |> List.Extra.greedyGroupsOf 4
    in
    list
        |> List.indexedMap
            (\cy ->
                List.indexedMap
                    (\cx ( msg, config ) ->
                        g
                            [ transform
                                [ Translate
                                    (toFloat cx)
                                    (toFloat cy)
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
            )
        |> List.concat
        |> (::)
            (textBlock { x = toFloat perRow / 2, y = 0 }
                (case model of
                    Nothing ->
                        []

                    Just selected ->
                        viewChanges selected
                )
            )
        |> svg
            [ viewBox
                -0.1
                -0.1
                (0.2 + toFloat perRow)
                (0.2 + toFloat (List.length list))
            , fontSize 0.2
            ]


viewChanges : ( Character, Graphics, Config ) -> List String
viewChanges ( character, graphics, config ) =
    let
        default : Config
        default =
            Avatars.characterToAvatar ( character, graphics )

        line : String -> (Config -> a) -> Maybe String
        line label acc =
            if acc default == acc config then
                Nothing

            else
                Just ("Changed " ++ label ++ " to " ++ Debug.toString (acc config))
    in
    [ line "Top" .top
    , line "Eyebrow" (.face >> .eyebrow)
    , line "Eyes" (.face >> .eyes)
    , line "Mouth" (.face >> .mouth)
    ]
        |> List.filterMap identity


allVariants :
    ( Character, Graphics, Config )
    -> List (List ( Msg, Config ))
allVariants ( character, graphics, { face } as current ) =
    let
        base : Config
        base =
            Avatars.characterToAvatar ( character, graphics )

        withTops : List (List Config)
        withTops =
            List.map
                (\top -> { current | top = top })
                (allTops current.top)
                |> makeGroups 3
    in
    (if base == current then
        [ ( Nothing, current )
        ]

     else
        [ ( Nothing, base )
        , ( Nothing, current )
        ]
    )
        :: (withTops
                ++ [ List.map
                        (\top -> { current | top = top })
                        (List.filterMap
                            (\accessory -> withAccessory accessory current.top)
                            allAccessories
                        )
                   , List.map
                        (\eyebrow ->
                            { current | face = { face | eyebrow = eyebrow } }
                        )
                        allEyebrows
                   , List.map
                        (\eyes ->
                            { current | face = { face | eyes = eyes } }
                        )
                        allEyes
                   , List.map
                        (\mouth ->
                            { current | face = { face | mouth = mouth } }
                        )
                        allMouths
                   ]
                |> List.map
                    (List.map
                        (\config ->
                            ( Just ( character, graphics, config )
                            , config
                            )
                        )
                    )
           )


textBlock : { x : Float, y : Float } -> List String -> Svg Msg
textBlock attrs lines =
    lines
        |> List.map
            (\line ->
                tspan
                    [ x attrs.x
                    , TypedSvg.Attributes.InEm.dy 1.2
                    ]
                    [ text line ]
            )
        |> centeredText
            [ x attrs.x
            , y attrs.y
            ]


makeGroups : Int -> List a -> List (List a)
makeGroups count list =
    let
        len : Int
        len =
            List.length list

        groupSize : Int
        groupSize =
            (len + count - 1) // count
    in
    List.Extra.greedyGroupsOf groupSize list


allEyebrows : List Eyebrow
allEyebrows =
    [ Eyebrow.Angry
    , Eyebrow.AngryNatural
    , Eyebrow.Default
    , Eyebrow.DefaultNatural
    , Eyebrow.FlatNatural
    , Eyebrow.RaisedExcited
    , Eyebrow.RaisedExcitedNatural
    , Eyebrow.SadConcerned
    , Eyebrow.SadConcernedNatural
    , Eyebrow.UnibrowNatural
    , Eyebrow.UpDown
    , Eyebrow.UpDownNatural
    ]


allMouths : List Mouth
allMouths =
    [ Mouth.Concerned
    , Mouth.Default
    , Mouth.Disbelief
    , Mouth.Eating
    , Mouth.Grimace
    , Mouth.Sad
    , Mouth.ScreamOpen
    , Mouth.Serious
    , Mouth.Smile
    , Mouth.Tongue
    , Mouth.Twinkle
    ]


allEyes : List Eyes
allEyes =
    [ Eyes.Close
    , Eyes.Cry
    , Eyes.Default
    , Eyes.Dizzy
    , Eyes.EyeRoll
    , Eyes.Happy
    , Eyes.Hearts
    , Eyes.Side
    , Eyes.Squint
    , Eyes.Surprised
    , Eyes.Wink
    , Eyes.WinkWacky
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


withAccessory : Accessory -> Top -> Maybe Top
withAccessory accessory top =
    case top of
        TopFacialHair _ _ ->
            Nothing

        TopHatColorAccessory t c _ ->
            Just <| TopHatColorAccessory t c accessory

        TopAccessoryFacialHair t _ f ->
            Just <| TopAccessoryFacialHair t accessory f

        TopHatColorAccessoryFacialHair t c _ f ->
            Just <| TopHatColorAccessoryFacialHair t c accessory f

        TopHairColorAccessoryFacialHair t c _ f ->
            Just <| TopHairColorAccessoryFacialHair t c accessory f


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


allAccessories : List Accessory
allAccessories =
    [ Accessory.Blank
    , Accessory.Kurt
    , Accessory.Prescription01
    , Accessory.Prescription02
    , Accessory.Round
    , Accessory.Sunglasses
    , Accessory.Wayfarers
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
