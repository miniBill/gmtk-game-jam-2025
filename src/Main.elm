module Main exposing (Model, Msg, main)

import Avataaars
import Avataaars.Graphics exposing (Graphics(..))
import Avatars
import Browser
import Color exposing (Color)
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
import Types exposing (Card(..), Character(..), Flags, Opponent, Player)


handSize : number
handSize =
    7


type Model
    = GeneratingSeed
    | PreparingHand
        { currentAvatar : ( Character, Graphics )
        , opponentHand : List (Card Opponent)
        , deck : List ( Card Player, Card Opponent )
        , hand : List (Card Player)
        , beingPlayed : List (Card Player)
        , mainSeed : Random.Seed
        }
    | PlayedHand
        { currentAvatar : ( Character, Graphics )
        , opponentHand : List (Card Opponent)
        , deck : List ( Card Player, Card Opponent )
        , hand : List (Card Player)
        , beingPlayed : List (Card Player)
        , mainSeed : Random.Seed
        }


type Msg
    = GeneratedSeed Random.Seed
    | Play (Card Player)
    | Unplay (Card Player)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( GeneratingSeed
    , let
        _ =
            Debug.todo
      in
      Random.initialSeed 413
        |> Random.constant
        |> Random.generate GeneratedSeed
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case ( msg, model ) of
        ( GeneratedSeed generatedSeed, GeneratingSeed ) ->
            let
                ( initialDeck, seed ) =
                    Random.step
                        (let
                            oneDeck : Random.Generator (List (Card kind))
                            oneDeck =
                                List.range 1 100
                                    |> List.map Card
                                    |> Random.List.shuffle
                         in
                         Random.map2 (List.map2 Tuple.pair) oneDeck oneDeck
                        )
                        generatedSeed

                ( ( hand, opponentHand ), deck ) =
                    List.Extra.splitAt handSize initialDeck
                        |> Tuple.mapFirst List.unzip

                newModel : Model
                newModel =
                    { currentAvatar = ( Karkat, Skull )
                    , hand = List.sortBy (\(Card c) -> c) hand
                    , beingPlayed = []
                    , opponentHand = List.sortBy (\(Card c) -> c) opponentHand
                    , deck = deck
                    , mainSeed = seed
                    }
                        |> PreparingHand
            in
            ( newModel
            , Cmd.none
            )

        ( _, GeneratingSeed ) ->
            ( model, Cmd.none )

        ( GeneratedSeed newSeed, _ ) ->
            ( model, Cmd.none )

        ( Play i, PreparingHand preparingModel ) ->
            ( { preparingModel
                | beingPlayed = preparingModel.beingPlayed ++ [ i ]
              }
                |> PreparingHand
            , Cmd.none
            )

        ( Unplay i, PreparingHand preparingModel ) ->
            ( { preparingModel
                | beingPlayed = List.Extra.remove i preparingModel.beingPlayed
              }
                |> PreparingHand
            , Cmd.none
            )

        ( _, PlayedHand playedModel ) ->
            ( model, Cmd.none )


view : Model -> TypedSvg.Core.Svg Msg
view model =
    case model of
        GeneratingSeed ->
            text "Loading..."

        PreparingHand preparingModel ->
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
                [ viewAvatar (Types.previous preparingModel.currentAvatar)
                , g [ transform [ Translate 1 0 ] ] [ viewOpponentCards { faceUp = False } preparingModel.mainSeed preparingModel.beingPlayed preparingModel.opponentHand ]
                ]
            , g
                [ id "playerGroup"
                , transform [ Translate 0 2 ]
                ]
                [ g [ transform [ Translate 0 1 ] ] [ viewAvatar preparingModel.currentAvatar ]
                , g [ transform [ Translate 1 0 ] ] (viewCards preparingModel.beingPlayed preparingModel.hand)
                ]
            , g [ id "deck", transform [ Translate 0 2 ] ]
                []
            ]
                |> svg
                    [ viewBox -border -border (w + border * 2) (h + border * 2)
                    , strokeWidth 0.05
                    , fontSize 0.25
                    ]

        PlayedHand playedModel ->
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
                [ viewAvatar (Types.previous playedModel.currentAvatar)
                , g [ transform [ Translate 1 0 ] ] [ viewOpponentCards { faceUp = True } playedModel.mainSeed playedModel.beingPlayed playedModel.opponentHand ]
                ]
            , g
                [ id "playerGroup"
                , transform [ Translate 0 2 ]
                ]
                [ g [ transform [ Translate 0 1 ] ] [ viewAvatar playedModel.currentAvatar ]
                , g [ transform [ Translate 1 0 ] ] (viewCards playedModel.beingPlayed playedModel.hand)
                ]
            , g [ id "deck", transform [ Translate 0 2 ] ]
                []
            ]
                |> svg
                    [ viewBox -border -border (w + border * 2) (h + border * 2)
                    , strokeWidth 0.05
                    , fontSize 0.25
                    ]


viewOpponentCards : { faceUp : Bool } -> Random.Seed -> List (Card Player) -> List (Card Opponent) -> Svg msg
viewOpponentCards { faceUp } seed beingPlayed opponentHand =
    let
        moveDown : Bool
        moveDown =
            List.length beingPlayed == handSize

        bestHand : List (Card Opponent)
        bestHand =
            if moveDown && not faceUp then
                calculateBestHand seed beingPlayed opponentHand

            else
                opponentHand
    in
    opponentHand
        |> List.map
            (\card ->
                viewCard []
                    { x =
                        List.Extra.elemIndex card bestHand
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
                    , opponent = True
                    }
            )
        |> g [ id "opponentCards" ]


calculateBestHand : Random.Seed -> List (Card Player) -> List (Card Opponent) -> List (Card Opponent)
calculateBestHand seed beingPlayed opponentHand =
    opponentHand
        |> shuffle seed
        |> permutations
        |> minimumBy (opponentScore beingPlayed)
        |> Maybe.withDefault opponentHand


permutations : List a -> List (List a)
permutations xs_ =
    case xs_ of
        [] ->
            [ [] ]

        xs ->
            let
                f : ( a, List a ) -> List (List a)
                f ( y, ys ) =
                    List.map ((::) y) (permutations ys)
            in
            List.concatMap f (select xs)


select : List a -> List ( a, List a )
select list =
    case list of
        [] ->
            []

        x :: xs ->
            ( x, xs ) :: List.map (\( y, ys ) -> ( y, x :: ys )) (select xs)


minimumBy : (a -> comparable) -> List a -> Maybe a
minimumBy f ls =
    let
        minBy : a -> ( a, comparable ) -> ( a, comparable )
        minBy x (( _, fy ) as min) =
            let
                fx : comparable
                fx =
                    f x
            in
            if fx < fy then
                ( x, fx )

            else
                min
    in
    case ls of
        [ l_ ] ->
            Just l_

        l_ :: ls_ ->
            Just <| Tuple.first <| List.foldl minBy ( l_, f l_ ) ls_

        _ ->
            Nothing


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
                                , opponent = False
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
                                , opponent = False
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
        , opponent : Bool
        }
    -> Svg msg
viewCard attrs config =
    let
        (Card c) =
            config.card

        cardRect : Svg msg
        cardRect =
            let
                border : Color
                border =
                    if config.opponent then
                        Color.white

                    else
                        Color.black

                cardBackground : Color
                cardBackground =
                    if config.faceUp then
                        Oklch.toColor
                            { alpha = 1
                            , lightness = 0.85
                            , chroma = 0.07
                            , hue = (toFloat c - 1) / 100
                            }

                    else
                        Color.darkGreen
            in
            rect
                [ x 0.1
                , y 0.1
                , width 0.5
                , height 0.8
                , rx 0.2
                , fill (Paint cardBackground)
                , stroke (Paint border)
                ]
                []
    in
    g
        (class [ "card" ]
            :: transform [ Translate config.x config.y ]
            :: style "transition: all 0.4s ease-in-out"
            :: attrs
        )
        (if config.faceUp then
            [ cardRect
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
            [ cardRect
            , let
                _ =
                    Debug.todo
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
