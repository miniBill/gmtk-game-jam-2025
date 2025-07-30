module Main exposing (Model, Msg, main)

import Avataaars
import Avataaars.Graphics exposing (Graphics(..))
import Avatars
import Browser
import Color exposing (Color)
import Color.Oklch as Oklch
import Hex
import List.Extra
import Random
import TypedSvg exposing (g, rect, svg, text_)
import TypedSvg.Attributes exposing (class, cursor, dominantBaseline, fill, id, stroke, style, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize, height, rx, strokeWidth, width, x, y)
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Events exposing (onClick)
import TypedSvg.Types exposing (AnchorAlignment(..), Cursor(..), DominantBaseline(..), Paint(..), Transform(..))
import Types exposing (Character(..), Flags)


type alias Model =
    { currentAvatar : ( Character, Graphics )
    , hand : List Int
    , beingPlayed : List Int
    , mainSeed : Random.Seed
    }


type Msg
    = GeneratedSeed Random.Seed
    | Play Int
    | Unplay Int


main : Program Flags (Maybe Model) Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : flags -> ( Maybe Model, Cmd Msg )
init _ =
    ( Nothing
    , let
        _ =
            Debug.todo
      in
      Random.initialSeed 0
        |> Random.constant
        |> Random.generate GeneratedSeed
    )


update : Msg -> Maybe Model -> ( Maybe Model, Cmd msg )
update msg maybeModel =
    case ( msg, maybeModel ) of
        ( GeneratedSeed seed, _ ) ->
            let
                newModel : Model
                newModel =
                    { currentAvatar = ( Aradia, Bat )
                    , hand =
                        List.range 1 10
                            |> List.map
                                (\i ->
                                    modBy 100 (i * 1337) + 1
                                )
                            |> List.sort
                    , beingPlayed = []
                    , mainSeed = seed
                    }
            in
            ( Just newModel
            , Cmd.none
            )

        ( _, Nothing ) ->
            ( maybeModel, Cmd.none )

        ( Play i, Just model ) ->
            ( { model
                | beingPlayed = model.beingPlayed ++ [ i ]
              }
                |> Just
            , Cmd.none
            )

        ( Unplay i, Just model ) ->
            ( { model
                | beingPlayed = List.Extra.remove i model.beingPlayed
              }
                |> Just
            , Cmd.none
            )


view : Maybe Model -> TypedSvg.Core.Svg Msg
view maybeModel =
    case maybeModel of
        Nothing ->
            text "Loading..."

        Just model ->
            let
                border : Float
                border =
                    0.1

                w : number
                w =
                    8

                h : number
                h =
                    4
            in
            [ rect
                [ x -border
                , y -border
                , width (w + border * 2)
                , height (h + border * 2)
                , fill (Paint (colorFromHex "#234000"))
                ]
                []
            , g [ id "opponentGroup" ]
                [ viewAvatar (Types.previous model.currentAvatar)
                ]
            , g
                [ id "playerGroup"
                , transform [ Translate 0 2 ]
                ]
                [ g [ transform [ Translate 0 1 ] ] [ viewAvatar model.currentAvatar ]
                , g [ transform [ Translate 1 0 ] ] (viewCards model.beingPlayed model.hand)
                ]
            ]
                |> svg
                    [ viewBox -border -border (w + border * 2) (h + border * 2)
                    , strokeWidth 0.05
                    , fontSize 0.25
                    ]


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


viewCards : List Int -> List Int -> List (Svg Msg)
viewCards beingPlayed cards =
    cards
        |> List.foldl
            (\card ( handX, acc ) ->
                let
                    inHand : Bool
                    inHand =
                        not (List.member card beingPlayed)

                    cardView : Svg Msg
                    cardView =
                        viewCard
                            [ onClick
                                (if inHand then
                                    Play card

                                 else
                                    Unplay card
                                )
                            , cursor CursorPointer
                            ]
                            (if inHand then
                                { x = handX
                                , y = 1
                                }

                             else
                                { x =
                                    List.Extra.elemIndex card beingPlayed
                                        |> Maybe.withDefault 0
                                        |> toFloat
                                        |> (*) 0.7
                                , y = 0
                                }
                            )
                            card
                in
                ( if inHand then
                    handX + 0.7

                  else
                    handX
                , cardView :: acc
                )
            )
            ( 0, [] )
        |> Tuple.second


viewCard : List (Attribute msg) -> { x : Float, y : Float } -> Int -> Svg msg
viewCard attrs coord c =
    g
        (class [ "card" ]
            :: transform [ Translate coord.x coord.y ]
            :: style "transition: all 0.4s ease-in-out"
            :: attrs
        )
        [ rect
            [ x 0.1
            , y 0.1
            , width 0.5
            , height 0.8
            , rx 0.2
            , fill
                (Paint
                    (Oklch.toColor
                        { alpha = 1
                        , lightness = 0.85
                        , chroma = 0.07
                        , hue = (toFloat c - 1) / 100
                        }
                    )
                )
            , stroke (Paint Color.black)
            ]
            []
        , text_
            [ x 0.35
            , y 0.5
            , textAnchor AnchorMiddle
            , dominantBaseline DominantBaselineCentral
            , fill (Paint Color.black)
            ]
            [ text (String.fromInt c) ]
        ]


viewAvatar : ( Character, Graphics ) -> Svg msg
viewAvatar character =
    Avataaars.view
        { width = 1
        , height = 1
        }
        (Avatars.characterToAvatar character)


subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none
