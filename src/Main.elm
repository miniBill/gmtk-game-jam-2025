module Main exposing (Model, Msg, main)

import Avataaars
import Avataaars.Graphics exposing (Graphics(..))
import Avatars
import Browser
import Color
import Color.Extra exposing (colorFromHex)
import Color.Oklch as Oklch
import List.Extra
import Random
import Random.List
import TypedSvg exposing (g, rect, svg, text_)
import TypedSvg.Attributes exposing (class, cursor, dominantBaseline, fill, id, stroke, style, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize, height, rx, strokeWidth, width, x, y)
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Events exposing (onClick)
import TypedSvg.Types exposing (AnchorAlignment(..), Cursor(..), DominantBaseline(..), Paint(..), Transform(..))
import Types exposing (Card(..), Character(..), Deck, Flags, Opponent, Player)


handSize : number
handSize =
    7


type alias Model =
    { currentAvatar : ( Character, Graphics )
    , opponentHand : List (Card Opponent)
    , deck : List (Card Deck)
    , hand : List (Card Player)
    , beingPlayed : List (Card Player)
    , mainSeed : Random.Seed
    }


type Msg
    = GeneratedSeed Random.Seed
    | Play (Card Player)
    | Unplay (Card Player)


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
      Random.initialSeed 413
        |> Random.constant
        |> Random.generate GeneratedSeed
    )


update : Msg -> Maybe Model -> ( Maybe Model, Cmd msg )
update msg maybeModel =
    case ( msg, maybeModel ) of
        ( GeneratedSeed generatedSeed, _ ) ->
            let
                ( initialDeck, seed ) =
                    Random.step
                        (List.range 1 100
                            |> Random.List.shuffle
                        )
                        generatedSeed

                ( hand, ( opponentHand, deck ) ) =
                    List.Extra.splitAt handSize initialDeck
                        |> Tuple.mapSecond (List.Extra.splitAt handSize)

                newModel : Model
                newModel =
                    { currentAvatar = ( Karkat, Skull )
                    , hand = List.map Card (List.sort hand)
                    , beingPlayed = []
                    , opponentHand = List.map Card (List.sort opponentHand)
                    , deck = List.map Card deck
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
                    6

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
                , g [ transform [ Translate 1 0 ] ] [ viewOpponentCards model.mainSeed model.beingPlayed model.opponentHand ]
                ]
            , g
                [ id "playerGroup"
                , transform [ Translate 0 2 ]
                ]
                [ g [ transform [ Translate 0 1 ] ] [ viewAvatar model.currentAvatar ]
                , g [ transform [ Translate 1 0 ] ] (viewCards model.beingPlayed model.hand)
                ]
            , g [ id "deck", transform [ Translate 0 2 ] ]
                []
            ]
                |> svg
                    [ viewBox -border -border (w + border * 2) (h + border * 2)
                    , strokeWidth 0.05
                    , fontSize 0.25
                    ]


viewOpponentCards : Random.Seed -> List (Card Player) -> List (Card Opponent) -> Svg msg
viewOpponentCards seed beingPlayed opponentHand =
    let
        moveDown : Bool
        moveDown =
            List.length beingPlayed == handSize

        shuffled : List (Card Opponent)
        shuffled =
            if moveDown then
                opponentHand
                    |> shuffle seed
                    |> List.Extra.permutations
                    |> List.Extra.minimumBy (opponentScore beingPlayed)
                    |> Maybe.withDefault opponentHand

            else
                opponentHand
    in
    opponentHand
        |> List.map
            (\card ->
                viewCard []
                    { x =
                        List.Extra.elemIndex card shuffled
                            |> Maybe.withDefault 0
                            |> toFloat
                            |> (*) 0.7
                    , y =
                        if moveDown then
                            1

                        else
                            0
                    , card = card
                    , faceUp = False
                    }
            )
        |> g [ id "opponentCards" ]


shuffle : Random.Seed -> List a -> List a
shuffle seed list =
    Random.step
        (Random.List.shuffle list)
        seed
        |> Tuple.first


opponentScore : List (Card Player) -> List (Card Opponent) -> Int
opponentScore playerHand opponentHand =
    20 - playerScore playerHand opponentHand


playerScore : List (Card Player) -> List (Card Opponent) -> Int
playerScore playerHand opponentHand =
    List.map2
        (\(Card playerCard) (Card opponentCard) ->
            case compare playerCard opponentCard of
                LT ->
                    0

                EQ ->
                    1

                GT ->
                    2
        )
        playerHand
        opponentHand
        |> List.sum


viewCards : List (Card Player) -> List (Card Player) -> List (Svg Msg)
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
                                , faceUp = True
                                , card = card
                                }

                             else
                                { x =
                                    List.Extra.elemIndex card beingPlayed
                                        |> Maybe.withDefault 0
                                        |> toFloat
                                        |> (*) 0.7
                                , y = 0
                                , faceUp = True
                                , card = card
                                }
                            )
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


viewCard :
    List (Attribute msg)
    ->
        { x : Float
        , y : Float
        , faceUp : Bool
        , card : Card kind
        }
    -> Svg msg
viewCard attrs config =
    g
        (class [ "card" ]
            :: transform [ Translate config.x config.y ]
            :: style "transition: all 0.4s ease-in-out"
            :: attrs
        )
        (if config.faceUp then
            let
                (Card c) =
                    config.card
            in
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

         else
            [ rect
                [ x 0.1
                , y 0.1
                , width 0.5
                , height 0.8
                , rx 0.2
                , fill (Paint Color.darkGreen)
                , stroke (Paint Color.black)
                ]
                []
            , let
                _ =
                    Debug.todo

                (Card c) =
                    config.card
              in
              text_
                [ x 0.35
                , y 0.5
                , textAnchor AnchorMiddle
                , dominantBaseline DominantBaselineCentral
                , fill (Paint Color.green)
                ]
                [ text (String.fromInt c) ]
            ]
        )


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
