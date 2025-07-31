module Main exposing (Model, Msg, main)

import Avataaars
import Avataaars.Graphics exposing (Graphics(..))
import Avatars
import Browser
import Color exposing (Color)
import Color.Extra exposing (colorFromHex)
import Color.Oklch as Oklch
import List.Extra
import Process
import Random
import Random.List
import Task
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (class, cursor, dominantBaseline, fill, id, stroke, style, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize, height, rx, strokeWidth, width, x, y)
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Events exposing (onClick)
import TypedSvg.Extra exposing (centeredText)
import TypedSvg.Types exposing (Cursor(..), DominantBaseline(..), Paint(..), Transform(..))
import Types exposing (Card, Character(..), Flags, Opponent, Player, opponentCard, playerCard)


handSize : number
handSize =
    7


roundsPerLoop : number
roundsPerLoop =
    5


deckSize : number
deckSize =
    handSize * roundsPerLoop


cardWidth : Float
cardWidth =
    0.5


cardHeight : Float
cardHeight =
    0.8


type Model
    = GeneratingSeed
    | PreparingHand
        { currentAvatar : ( Character, Graphics )
        , initialDeck : List ( Card Player, Card Opponent )
        , playerHand : List (Card Player)
        , playerChoices : List (Card Player)
        , opponentHand : List (Card Opponent)
        , discardPile : List ( Card Player, Card Opponent )
        , mainSeed : Random.Seed
        }
    | PlayedHand
        { currentAvatar : ( Character, Graphics )
        , initialDeck : List ( Card Player, Card Opponent )
        , play : List ( Card Player, Card Opponent )
        , discardPile : List ( Card Player, Card Opponent )
        , mainSeed : Random.Seed
        }


type Msg
    = GeneratedSeed Random.Seed
    | Play (Card Player)
    | Unplay (Card Player)
    | SubmitHand
    | NextRound


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GeneratedSeed generatedSeed, GeneratingSeed ) ->
            let
                ( initialDeck, seed ) =
                    Random.step
                        (let
                            oneDeck : (Int -> Card kind) -> Random.Generator (List (Card kind))
                            oneDeck f =
                                List.range 1 deckSize
                                    |> List.map f
                                    |> Random.List.shuffle
                         in
                         Random.map2 (List.map2 Tuple.pair) (oneDeck playerCard) (oneDeck opponentCard)
                        )
                        generatedSeed

                ( hand, opponentHand ) =
                    List.take handSize initialDeck
                        |> List.unzip

                newModel : Model
                newModel =
                    { currentAvatar = ( Karkat, Skull )
                    , initialDeck = initialDeck
                    , playerHand = List.sortBy Types.cardValue hand
                    , playerChoices = []
                    , opponentHand = List.sortBy Types.cardValue opponentHand
                    , discardPile = []
                    , mainSeed = seed
                    }
                        |> PreparingHand
            in
            ( newModel
            , Cmd.none
            )

        ( Play i, PreparingHand preparingModel ) ->
            ( { preparingModel
                | playerChoices = preparingModel.playerChoices ++ [ i ]
              }
                |> PreparingHand
            , Cmd.none
            )

        ( Unplay i, PreparingHand preparingModel ) ->
            ( { preparingModel
                | playerChoices = List.Extra.remove i preparingModel.playerChoices
              }
                |> PreparingHand
            , Cmd.none
            )

        ( SubmitHand, PreparingHand preparingModel ) ->
            if List.length preparingModel.playerChoices == handSize then
                let
                    opponentHand : List (Card Opponent)
                    opponentHand =
                        calculateBestHand
                            preparingModel.mainSeed
                            preparingModel.playerChoices
                            preparingModel.opponentHand
                in
                ( PlayedHand
                    { currentAvatar = preparingModel.currentAvatar
                    , initialDeck = preparingModel.initialDeck
                    , play =
                        List.map2 Tuple.pair
                            preparingModel.playerChoices
                            opponentHand
                    , discardPile = preparingModel.discardPile
                    , mainSeed = preparingModel.mainSeed
                    }
                , Process.sleep 2000 |> Task.perform (\_ -> NextRound)
                )

            else
                ( model, Cmd.none )

        ( NextRound, PlayedHand playedModel ) ->
            let
                ( playerHand, opponentHand ) =
                    playedModel.initialDeck
                        |> List.drop (handSize + List.length playedModel.discardPile)
                        |> List.take handSize
                        |> List.unzip
            in
            ( PreparingHand
                { currentAvatar = playedModel.currentAvatar
                , initialDeck = playedModel.initialDeck
                , discardPile = playedModel.discardPile ++ playedModel.play
                , mainSeed = playedModel.mainSeed
                , opponentHand = List.sortBy Types.cardValue opponentHand
                , playerHand = List.sortBy Types.cardValue playerHand
                , playerChoices = []
                }
            , Cmd.none
            )

        _ ->
            let
                _ =
                    Debug.log "Ignoring (msg,model)" ( msg, model )
            in
            ( model, Cmd.none )


view : Model -> TypedSvg.Core.Svg Msg
view model =
    let
        border : Float
        border =
            0.1

        gameWidth : number
        gameWidth =
            7

        gameHeight : number
        gameHeight =
            4

        backgroundRect : Svg msg
        backgroundRect =
            rect
                [ x -border
                , y -border
                , width (gameWidth + border * 2)
                , height (gameHeight + border * 2)
                , fill (Paint (colorFromHex "#234000"))
                ]
                []

        children : List (Svg Msg)
        children =
            case model of
                GeneratingSeed ->
                    [ text "Loading..." ]

                PreparingHand preparingModel ->
                    [ backgroundRect
                    , g [ id "opponentGroup" ]
                        [ viewAvatar (Types.previous preparingModel.currentAvatar)
                        , g [ transform [ Translate 6 0 ] ] (viewScore opponentScore preparingModel.discardPile Nothing)
                        ]
                    , g
                        [ id "playerGroup"
                        , transform [ Translate 0 2 ]
                        ]
                      <|
                        List.filterMap identity
                            [ Just <| g [ transform [ Translate 0 1 ] ] [ viewAvatar preparingModel.currentAvatar ]
                            , Just <| g [ transform [ Translate 6 1 ] ] (viewScore playerScore preparingModel.discardPile Nothing)
                            , submitHandButton preparingModel
                            ]
                    , g [ id "cards" ] (viewCards model)
                    ]

                PlayedHand playedModel ->
                    [ backgroundRect
                    , g [ id "opponentGroup" ]
                        [ viewAvatar (Types.previous playedModel.currentAvatar)
                        , g [ transform [ Translate 6 0 ] ] (viewScore opponentScore playedModel.discardPile (Just playedModel.play))
                        ]
                    , g
                        [ id "playerGroup"
                        , transform [ Translate 0 2 ]
                        ]
                        [ g [ transform [ Translate 0 1 ] ] [ viewAvatar playedModel.currentAvatar ]
                        , g [ transform [ Translate 6 1 ] ] (viewScore playerScore playedModel.discardPile (Just playedModel.play))
                        ]
                    , g [ id "cards" ] (viewCards model)
                    ]
    in
    svg
        [ viewBox -border -border (gameWidth + border * 2) (gameHeight + border * 2)
        , strokeWidth 0.05
        , fontSize 0.25
        , style "transform: perspective(1)"
        ]
        children


opponentScore : List ( Card Player, Card Opponent ) -> Float
opponentScore pile =
    toFloat (List.length pile) - playerScore pile


viewScore : (List ( Card Player, Card Opponent ) -> Float) -> List ( Card Player, Card Opponent ) -> Maybe (List ( Card Player, Card Opponent )) -> List (Svg Msg)
viewScore toScore discard play =
    [ centeredText
        [ x 0.5
        , y 0.45
        , fill (Paint Color.white)
        , dominantBaseline DominantBaselineAuto
        ]
        [ text "Score" ]
    , centeredText
        [ x 0.5
        , y 0.55
        , fill (Paint Color.white)
        , dominantBaseline DominantBaselineHanging
        ]
        [ case play of
            Nothing ->
                text (String.fromFloat (toScore discard))

            Just p ->
                text (String.fromFloat (toScore discard) ++ " => " ++ String.fromFloat (toScore (discard ++ p)))
        ]
    ]


viewCards : Model -> List (Svg Msg)
viewCards model =
    (case model of
        GeneratingSeed ->
            []

        PreparingHand { initialDeck } ->
            initialDeck

        PlayedHand { initialDeck } ->
            initialDeck
    )
        |> List.reverse
        |> List.concatMap
            (\( playerCard, opponentCard ) ->
                [ viewPlayerCard model playerCard
                , viewOpponentCard model opponentCard
                ]
            )


viewPlayerCard : Model -> Card Player -> Svg Msg
viewPlayerCard model card =
    let
        deck :
            { model | initialDeck : List ( Card Player, Card Opponent ) }
            -> ( () -> Maybe Int, Int -> Svg msg )
        deck innerModel =
            ( \_ -> List.Extra.findIndex (\( c, _ ) -> c == card) innerModel.initialDeck
            , \index ->
                viewCard []
                    { x = deckLerp index
                    , y = 2
                    , card = card
                    , faceUp = False
                    }
            )

        discardPile :
            { model
                | initialDeck : List ( Card Player, Card Opponent )
                , discardPile : List ( Card Player, Card Opponent )
            }
            -> ( () -> Maybe Int, Int -> Svg msg )
        discardPile innerModel =
            ( \_ ->
                if List.any (\( c, _ ) -> c == card) innerModel.discardPile then
                    List.Extra.findIndex (\( c, _ ) -> c == card) innerModel.initialDeck

                else
                    Nothing
            , \index ->
                viewCard []
                    { x = 6 + deckLerp index
                    , y = 2
                    , card = card
                    , faceUp = False
                    }
            )
    in
    case model of
        GeneratingSeed ->
            text ""

        PreparingHand preparingModel ->
            findFirst
                [ ( \_ -> List.Extra.elemIndex card preparingModel.playerChoices
                  , \index ->
                        viewCard
                            [ onClick (Unplay card)
                            , cursor CursorPointer
                            ]
                            { x = 1 + toFloat index * (cardWidth + 0.2)
                            , y = 2
                            , card = card
                            , faceUp = True
                            }
                  )
                , ( \_ ->
                        let
                            reducedHand : List (Card Player)
                            reducedHand =
                                List.Extra.removeWhen
                                    (\c -> List.member c preparingModel.playerChoices)
                                    preparingModel.playerHand
                        in
                        List.Extra.elemIndex card reducedHand
                  , \index ->
                        viewCard
                            [ onClick (Play card)
                            , cursor CursorPointer
                            ]
                            { x = 1 + toFloat index * (cardWidth + 0.2)
                            , y = 3
                            , card = card
                            , faceUp = True
                            }
                  )
                , discardPile preparingModel
                , deck preparingModel
                ]

        PlayedHand playedModel ->
            findFirst
                [ ( \_ -> List.Extra.findIndex (\( c, _ ) -> c == card) playedModel.play
                  , \index ->
                        viewCard []
                            { x = 1 + toFloat index * (cardWidth + 0.2)
                            , y = 2
                            , card = card
                            , faceUp = True
                            }
                  )
                , discardPile playedModel
                , deck playedModel
                ]


findFirst : List ( () -> Maybe b, b -> Svg msg ) -> Svg msg
findFirst list =
    list
        |> List.Extra.findMap (\( f, g ) -> Maybe.map g (f ()))
        |> Maybe.withDefault (text "")


viewOpponentCard : Model -> Card Opponent -> Svg Msg
viewOpponentCard model card =
    let
        deck : { a | initialDeck : List ( Card Player, Card Opponent ) } -> ( () -> Maybe Int, Int -> Svg msg )
        deck innerModel =
            ( \_ -> List.Extra.findIndex (\( _, c ) -> c == card) innerModel.initialDeck
            , \index ->
                viewCard []
                    { x = deckLerp index
                    , y = 1
                    , card = card
                    , faceUp = False
                    }
            )

        discardPile :
            { model
                | initialDeck : List ( Card Player, Card Opponent )
                , discardPile : List ( Card Player, Card Opponent )
            }
            -> ( () -> Maybe Int, Int -> Svg msg )
        discardPile innerModel =
            ( \_ ->
                if List.any (\( _, c ) -> c == card) innerModel.discardPile then
                    List.Extra.findIndex (\( _, c ) -> c == card) innerModel.initialDeck

                else
                    Nothing
            , \index ->
                viewCard []
                    { x = 6 + deckLerp index
                    , y = 1
                    , card = card
                    , faceUp = False
                    }
            )
    in
    case model of
        GeneratingSeed ->
            text ""

        PreparingHand preparingModel ->
            findFirst
                [ ( \_ -> List.Extra.elemIndex card preparingModel.opponentHand
                  , \i ->
                        viewCard []
                            { x = 1 + toFloat i * (cardWidth + 0.2)
                            , y = 0
                            , card = card
                            , faceUp = False
                            }
                  )
                , discardPile preparingModel
                , deck preparingModel
                ]

        PlayedHand playedModel ->
            findFirst
                [ ( \_ -> List.Extra.findIndex (\( _, c ) -> c == card) playedModel.play
                  , \i ->
                        viewCard []
                            { x = 1 + toFloat i * (cardWidth + 0.2)
                            , y = 1
                            , card = card
                            , faceUp = True
                            }
                  )
                , discardPile playedModel
                , deck playedModel
                ]


deckLerp : Int -> Float
deckLerp index =
    (toFloat index - 1) / deckSize * (1 - cardWidth) - 0.1


submitHandButton : { a | playerChoices : List (Card Player) } -> Maybe (Svg Msg)
submitHandButton preparingModel =
    if List.length preparingModel.playerChoices == handSize then
        Just <|
            g
                [ transform [ Translate 1 1 ]
                , onClick SubmitHand
                ]
                [ rect
                    [ x 0.1
                    , y 0.1
                    , width ((cardWidth + 0.2) * handSize - 0.2)
                    , height cardHeight
                    , fill (Paint Color.orange)
                    , rx 0.2
                    , style
                        (String.join "; "
                            [ "transition: all 0.4s ease-in-out"
                            , "filter: drop-shadow(0.01px 0.02px 0.02px rgb(0 0 0 / 0.4))"
                            ]
                        )
                    ]
                    []
                , centeredText
                    [ x ((cardWidth + 0.2) * handSize / 2 - 0.1)
                    , y 0.5
                    ]
                    [ text "Submit hand" ]
                ]

    else
        Nothing


calculateBestHand : Random.Seed -> List (Card Player) -> List (Card Opponent) -> List (Card Opponent)
calculateBestHand seed beingPlayed opponentHand =
    opponentHand
        |> shuffle seed
        |> permutations
        |> minimumBy (\hand -> playerScore (List.map2 Tuple.pair beingPlayed hand))
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


playerScore : List ( Card Player, Card Opponent ) -> Float
playerScore pile =
    List.map
        (\( playerCard, opponentCard ) ->
            case compare (Types.cardValue playerCard) (Types.cardValue opponentCard) of
                LT ->
                    0

                EQ ->
                    0.5

                GT ->
                    1
        )
        pile
        |> List.sum


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
    let
        margin : Float
        margin =
            0.1

        cardRect : Svg msg
        cardRect =
            let
                border : Color
                border =
                    if Types.isOpponentCard config.card then
                        Color.white

                    else
                        Color.charcoal

                cardBackground : Color
                cardBackground =
                    if config.faceUp then
                        Oklch.toColor
                            { alpha = 1
                            , lightness = 0.85
                            , chroma = 0.07
                            , hue = (toFloat (Types.cardValue config.card) - 1) / deckSize
                            }

                    else
                        Color.darkGreen
            in
            rect
                [ x margin
                , y margin
                , width cardWidth
                , height cardHeight
                , rx 0.2
                , fill (Paint cardBackground)
                , stroke (Paint border)
                ]
                []
    in
    g
        (class [ "card" ]
            :: transform [ Translate config.x config.y ]
            :: style
                (String.join "; "
                    [ "transition: all 0.4s ease-in-out"
                    , "filter: drop-shadow(0.01px 0.03px 0.02px rgb(0 0 0 / 0.4))"
                    ]
                )
            :: attrs
        )
        (if config.faceUp then
            [ cardRect
            , centeredText
                [ x (cardWidth / 2 + margin)
                , y (cardHeight / 2 + margin)
                , fill (Paint Color.black)
                ]
                [ text (String.fromInt (Types.cardValue config.card)) ]
            ]

         else
            [ cardRect
            , let
                _ =
                    Debug.todo
              in
              centeredText
                [ x (cardWidth / 2 + margin)
                , y (cardHeight / 2 + margin)
                , fill (Paint Color.green)
                ]
                [ text (String.fromInt (Types.cardValue config.card)) ]
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
