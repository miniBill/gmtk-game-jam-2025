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
    | InGame
        { currentAvatar : ( Character, Graphics )
        , initialDeck : List ( Card Player, Card Opponent )
        , discardPile : List ( Card Player, Card Opponent )
        , mainSeed : Random.Seed
        , game : Game
        }


type Game
    = DrawingInitialHand
    | PreparingHand
        { playerHand : List (Card Player)
        , playerChoices : List (Card Player)
        , opponentHand : List (Card Opponent)
        }
    | PlayedHand
        { play : List ( Card Player, Card Opponent )
        }
    | GameFinished


type Msg
    = GeneratedSeed Random.Seed
    | GameMsg GameMsg


type GameMsg
    = Play (Card Player)
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

                newModel : Model
                newModel =
                    InGame
                        { currentAvatar = ( Karkat, Skull )
                        , initialDeck = initialDeck
                        , discardPile = []
                        , mainSeed = seed
                        , game = DrawingInitialHand
                        }
            in
            ( newModel
            , Process.sleep 2000 |> Task.perform (\_ -> GameMsg NextRound)
            )

        ( GameMsg gameMsg, InGame inGameModel ) ->
            let
                stillInGame : Game -> Model
                stillInGame inner =
                    InGame { inGameModel | game = inner }
            in
            case ( gameMsg, inGameModel.game ) of
                ( Play i, PreparingHand preparingModel ) ->
                    ( { preparingModel
                        | playerChoices = preparingModel.playerChoices ++ [ i ]
                      }
                        |> PreparingHand
                        |> stillInGame
                    , Cmd.none
                    )

                ( Play _, _ ) ->
                    ( model, Cmd.none )

                ( Unplay i, PreparingHand preparingModel ) ->
                    ( { preparingModel
                        | playerChoices = List.Extra.remove i preparingModel.playerChoices
                      }
                        |> PreparingHand
                        |> stillInGame
                    , Cmd.none
                    )

                ( Unplay _, _ ) ->
                    ( model, Cmd.none )

                ( SubmitHand, PreparingHand preparingModel ) ->
                    if List.length preparingModel.playerChoices == handSize then
                        let
                            opponentHand : List (Card Opponent)
                            opponentHand =
                                calculateBestHand
                                    inGameModel.mainSeed
                                    preparingModel.playerChoices
                                    preparingModel.opponentHand
                        in
                        ( { play =
                                List.map2 Tuple.pair
                                    preparingModel.playerChoices
                                    opponentHand
                          }
                            |> PlayedHand
                            |> stillInGame
                        , let
                            _ =
                                Debug.todo
                          in
                          Process.sleep 50000 |> Task.perform (\_ -> GameMsg NextRound)
                        )

                    else
                        ( model, Cmd.none )

                ( SubmitHand, _ ) ->
                    ( model, Cmd.none )

                ( NextRound, DrawingInitialHand ) ->
                    let
                        ( playerHand, opponentHand ) =
                            inGameModel.initialDeck
                                |> List.take handSize
                                |> List.unzip
                    in
                    ( InGame
                        { inGameModel
                            | game =
                                { opponentHand = List.sortBy Types.cardValue opponentHand
                                , playerHand = List.sortBy Types.cardValue playerHand
                                , playerChoices = []
                                }
                                    |> PreparingHand
                        }
                    , Cmd.none
                    )

                ( NextRound, PlayedHand playedModel ) ->
                    let
                        ( playerHand, opponentHand ) =
                            inGameModel.initialDeck
                                |> List.drop (handSize + List.length inGameModel.discardPile)
                                |> List.take handSize
                                |> List.unzip
                    in
                    ( InGame
                        { inGameModel
                            | discardPile = inGameModel.discardPile ++ playedModel.play
                            , game =
                                { opponentHand = List.sortBy Types.cardValue opponentHand
                                , playerHand = List.sortBy Types.cardValue playerHand
                                , playerChoices = []
                                }
                                    |> PreparingHand
                        }
                    , Cmd.none
                    )

                ( NextRound, _ ) ->
                    ( model, Cmd.none )

        ( _, _ ) ->
            let
                _ =
                    Debug.log "Wrong combination" ( msg, model )
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

        children : List (Svg Msg)
        children =
            case model of
                GeneratingSeed ->
                    [ text "Loading..." ]

                InGame inGameModel ->
                    let
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
                    in
                    backgroundRect
                        :: viewAvatar (Types.previous inGameModel.currentAvatar)
                        :: g [ transform [ Translate 0 3 ] ] [ viewAvatar inGameModel.currentAvatar ]
                        :: (case inGameModel.game of
                                DrawingInitialHand ->
                                    [ g [ transform [ Translate 6 0 ] ] (viewScore opponentScore inGameModel.discardPile Nothing)
                                    , g [ transform [ Translate 6 3 ] ] (viewScore playerScore inGameModel.discardPile Nothing)
                                    ]

                                PreparingHand preparingModel ->
                                    [ g [ transform [ Translate 6 0 ] ] (viewScore opponentScore inGameModel.discardPile Nothing)
                                    , g
                                        [ transform [ Translate 0 2 ]
                                        ]
                                      <|
                                        List.filterMap identity
                                            [ Just <| g [ transform [ Translate 6 1 ] ] (viewScore playerScore inGameModel.discardPile Nothing)
                                            , submitHandButton preparingModel
                                            ]
                                    ]

                                PlayedHand playedModel ->
                                    [ g [ transform [ Translate 6 0 ] ] (viewScore opponentScore inGameModel.discardPile (Just playedModel.play))
                                    , g [ transform [ Translate 6 3 ] ] (viewScore playerScore inGameModel.discardPile (Just playedModel.play))
                                    ]

                                GameFinished ->
                                    [ g [ transform [ Translate 6 0 ] ] (viewScore opponentScore inGameModel.discardPile Nothing)
                                    , g [ transform [ Translate 6 3 ] ] (viewScore playerScore inGameModel.discardPile Nothing)
                                    ]
                           )
                        ++ [ g [ id "cards" ] (viewCards model) ]
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

        InGame { initialDeck } ->
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
    case model of
        GeneratingSeed ->
            text ""

        InGame inGameModel ->
            let
                viewIfInDeck : ( () -> Maybe Int, Int -> Svg msg )
                viewIfInDeck =
                    ( \_ -> List.Extra.findIndex (\( c, _ ) -> c == card) inGameModel.initialDeck
                    , \index ->
                        viewCard []
                            { x = deckLerp index
                            , y = 2
                            , card = card
                            , faceUp = False
                            }
                    )

                viewIfInDiscardPile : ( () -> Maybe Int, Int -> Svg msg )
                viewIfInDiscardPile =
                    ( \_ ->
                        if List.any (\( c, _ ) -> c == card) inGameModel.discardPile then
                            List.Extra.findIndex (\( c, _ ) -> c == card) inGameModel.initialDeck

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

                specific : List ( () -> Maybe Int, Int -> Svg GameMsg )
                specific =
                    case inGameModel.game of
                        DrawingInitialHand ->
                            []

                        PreparingHand preparingModel ->
                            let
                                viewIfSelected : ( () -> Maybe Int, Int -> Svg GameMsg )
                                viewIfSelected =
                                    ( \_ -> List.Extra.elemIndex card preparingModel.playerChoices
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

                                viewIfInHand : ( () -> Maybe Int, Int -> Svg GameMsg )
                                viewIfInHand =
                                    ( \_ ->
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
                            in
                            [ viewIfSelected
                            , viewIfInHand
                            ]

                        PlayedHand playedModel ->
                            let
                                viewIfInPlay : ( () -> Maybe Int, Int -> Svg msg )
                                viewIfInPlay =
                                    ( \_ -> List.Extra.findIndex (\( c, _ ) -> c == card) playedModel.play
                                    , \index ->
                                        viewCard []
                                            { x = 1 + toFloat index * (cardWidth + 0.2)
                                            , y = 2
                                            , card = card
                                            , faceUp = True
                                            }
                                    )
                            in
                            [ viewIfInPlay ]

                        GameFinished ->
                            []
            in
            findFirst (specific ++ [ viewIfInDiscardPile, viewIfInDeck ])
                |> TypedSvg.Core.map GameMsg


findFirst :
    List
        ( () -> Maybe b
        , b -> Svg msg
        )
    -> Svg msg
findFirst list =
    list
        |> List.Extra.findMap (\( f, g ) -> Maybe.map g (f ()))
        |> Maybe.withDefault (text "")


viewOpponentCard : Model -> Card Opponent -> Svg Msg
viewOpponentCard model card =
    case model of
        GeneratingSeed ->
            text ""

        InGame inGameModel ->
            let
                deck : ( () -> Maybe Int, Int -> Svg msg )
                deck =
                    ( \_ -> List.Extra.findIndex (\( _, c ) -> c == card) inGameModel.initialDeck
                    , \index ->
                        viewCard []
                            { x = deckLerp index
                            , y = 1
                            , card = card
                            , faceUp = False
                            }
                    )

                discardPile : ( () -> Maybe Int, Int -> Svg msg )
                discardPile =
                    ( \_ ->
                        if List.any (\( _, c ) -> c == card) inGameModel.discardPile then
                            List.Extra.findIndex (\( _, c ) -> c == card) inGameModel.initialDeck

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

                specific : List ( () -> Maybe Int, Int -> Svg msg )
                specific =
                    case inGameModel.game of
                        DrawingInitialHand ->
                            []

                        PreparingHand preparingModel ->
                            [ ( \_ -> List.Extra.elemIndex card preparingModel.opponentHand
                              , \i ->
                                    viewCard []
                                        { x = 1 + toFloat i * (cardWidth + 0.2)
                                        , y = 0
                                        , card = card
                                        , faceUp = False
                                        }
                              )
                            ]

                        PlayedHand playedModel ->
                            [ ( \_ -> List.Extra.findIndex (\( _, c ) -> c == card) playedModel.play
                              , \i ->
                                    viewCard []
                                        { x = 1 + toFloat i * (cardWidth + 0.2)
                                        , y = 1
                                        , card = card
                                        , faceUp = True
                                        }
                              )
                            ]

                        GameFinished ->
                            []
            in
            findFirst (specific ++ [ discardPile, deck ])


deckLerp : Int -> Float
deckLerp index =
    (toFloat index - 1) / deckSize * (1 - cardWidth) - 0.1


submitHandButton : { a | playerChoices : List (Card Player) } -> Maybe (Svg Msg)
submitHandButton preparingModel =
    if List.length preparingModel.playerChoices == handSize then
        Just <|
            g
                [ transform [ Translate 1 1 ]
                , onClick (GameMsg SubmitHand)
                ]
                [ rect
                    [ x 0.1
                    , y 0.1
                    , width ((cardWidth + 0.2) * handSize - 0.2)
                    , height cardHeight
                    , fill (Paint Color.orange)
                    , rx 0.1
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
                , rx 0.1
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
