module Main exposing (Model, Msg, main)

import Avataaars
import Avataaars.Graphics exposing (Graphics)
import Avatars
import Browser
import CardSet exposing (CardSet)
import Color exposing (Color)
import Color.Extra exposing (colorFromHex)
import Format
import List.Extra
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Random
import Random.List
import TypedSvg exposing (g, rect, svg, tspan)
import TypedSvg.Attributes exposing (class, cursor, dominantBaseline, fill, id, stroke, style, transform, viewBox)
import TypedSvg.Attributes.InEm
import TypedSvg.Attributes.InPx exposing (fontSize, height, rx, strokeWidth, width, x, y)
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Events exposing (onClick)
import TypedSvg.Extra exposing (centeredText)
import TypedSvg.Types exposing (Cursor(..), DominantBaseline(..), Paint(..), Transform(..))
import Types exposing (Card(..), Character, Flags, Opponent, Player, Suit(..))


handSize : number
handSize =
    5


handCount : Int
handCount =
    List.length deck // handSize


deck : List (Card kind)
deck =
    List.Extra.lift2 Card allSuits (List.range 6 14)


deckSize : Int
deckSize =
    List.length deck


allSuits : List Suit
allSuits =
    [ Hearts, Bells, Leaves, Acorns ]


cardWidth : Float
cardWidth =
    0.5


cardHeight : Float
cardHeight =
    0.8


type alias InGameModel =
    { currentAvatar : Avatar
    , playedHands : List OpposedHands
    , seed : Random.Seed
    , game : GameStep
    , replaying : List (CardSet Opponent)
    , previousGames : List Game
    }


type alias Game =
    List OpposedHands


type alias Avatar =
    ( Character, Graphics )


type alias OpposedHands =
    ( CardSet Player
    , CardSet Opponent
    )


type Model
    = GeneratingSeed
    | PickingAvatar Random.Seed
    | InGame InGameModel


type GameStep
    = PreparingHand (CardSet Player) (CardSet Opponent)
    | PlayedHand (CardSet Player) (CardSet Opponent)
    | GameFinished


type Msg
    = GeneratedSeed Random.Seed
    | PickedAvatar Avatar
    | GameMsg GameMsg


type GameMsg
    = Select (Card Player)
    | Unselect (Card Player)
    | SubmitHand
    | NextRound
    | NextLoop
    | NextGame


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
    , Random.independentSeed |> Random.generate GeneratedSeed
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GeneratedSeed generatedSeed, GeneratingSeed ) ->
            ( PickingAvatar generatedSeed, Cmd.none )

        ( GeneratedSeed _, _ ) ->
            ( model, Cmd.none )

        ( PickedAvatar avatar, PickingAvatar generatedSeed ) ->
            let
                ( opponentGame, newSeed ) =
                    Random.step opponentGameGenerator generatedSeed
            in
            ( initGame avatar newSeed opponentGame, Cmd.none )

        ( PickedAvatar _, _ ) ->
            ( model, Cmd.none )

        ( GameMsg gameMsg, InGame inGameModel ) ->
            let
                stillInGame : GameStep -> Model
                stillInGame inner =
                    InGame { inGameModel | game = inner }
            in
            case ( gameMsg, inGameModel.game ) of
                ( Select i, PreparingHand playerChoices opponentHand ) ->
                    ( PreparingHand (CardSet.insert i playerChoices) opponentHand
                        |> stillInGame
                    , Cmd.none
                    )

                ( Select _, _ ) ->
                    ( model, Cmd.none )

                ( Unselect i, PreparingHand playerChoices opponentHand ) ->
                    ( PreparingHand (CardSet.remove i playerChoices) opponentHand
                        |> stillInGame
                    , Cmd.none
                    )

                ( Unselect _, _ ) ->
                    ( model, Cmd.none )

                ( SubmitHand, PreparingHand playerChoices opponentChoices ) ->
                    if CardSet.size playerChoices == handSize then
                        ( PlayedHand playerChoices opponentChoices
                            |> stillInGame
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                ( SubmitHand, _ ) ->
                    ( model, Cmd.none )

                ( NextRound, PlayedHand playerHand opponentHand ) ->
                    case inGameModel.replaying of
                        [] ->
                            ( InGame
                                { inGameModel
                                    | playedHands = ( playerHand, opponentHand ) :: inGameModel.playedHands
                                    , game = GameFinished
                                }
                            , Cmd.none
                            )

                        newOpponentHand :: replayingQueue ->
                            ( InGame
                                { inGameModel
                                    | playedHands = ( playerHand, opponentHand ) :: inGameModel.playedHands
                                    , replaying = replayingQueue
                                    , game = PreparingHand CardSet.empty newOpponentHand
                                }
                            , Cmd.none
                            )

                ( NextRound, _ ) ->
                    ( model, Cmd.none )

                ( NextLoop, GameFinished ) ->
                    let
                        ( firstOpponentHand, replaying ) =
                            -- This is intentionally reversing, `playedHands` is most-recent-first,
                            -- `replaying` is oldest-first
                            case
                                List.foldl
                                    (\( c, _ ) a -> CardSet.giveToOpponent c :: a)
                                    []
                                    inGameModel.playedHands
                            of
                                [] ->
                                    ( CardSet.empty, [] )

                                h :: t ->
                                    ( h, t )
                    in
                    ( InGame
                        { currentAvatar = Types.next inGameModel.currentAvatar
                        , playedHands = []
                        , replaying = replaying
                        , previousGames = inGameModel.playedHands :: inGameModel.previousGames
                        , game = PreparingHand CardSet.empty firstOpponentHand
                        , seed = inGameModel.seed
                        }
                    , Cmd.none
                    )

                ( NextLoop, _ ) ->
                    ( model, Cmd.none )

                ( NextGame, GameFinished ) ->
                    let
                        avatar : Avatar
                        avatar =
                            Types.next (Types.next inGameModel.currentAvatar)

                        ( opponentGame, newSeed ) =
                            Random.step opponentGameGenerator inGameModel.seed
                    in
                    ( initGame avatar newSeed opponentGame
                    , Cmd.none
                    )

                ( NextGame, _ ) ->
                    ( model, Cmd.none )

        ( GameMsg _, _ ) ->
            ( model, Cmd.none )


opponentGameGenerator : Random.Generator (NonEmpty (CardSet Opponent))
opponentGameGenerator =
    Random.constant ()
        |> Random.andThen (\_ -> Debug.todo "generateOpponentGame")


initGame : Avatar -> Random.Seed -> NonEmpty (CardSet Opponent) -> Model
initGame avatar generatedSeed opponentGame =
    InGame
        { currentAvatar = avatar
        , playedHands = []
        , seed = generatedSeed
        , game = PreparingHand CardSet.empty (NonEmpty.head opponentGame)
        , previousGames = []
        , replaying = NonEmpty.tail opponentGame
        }


view : Model -> TypedSvg.Core.Svg Msg
view model =
    let
        border : Float
        border =
            0.1

        gameWidth : Float
        gameWidth =
            7.5

        gameHeight : Float
        gameHeight =
            8

        children : List (Svg Msg)
        children =
            case model of
                GeneratingSeed ->
                    [ text "Loading..." ]

                PickingAvatar generatedSeed ->
                    let
                        scale : Float
                        scale =
                            0.9

                        perRow : number
                        perRow =
                            6
                    in
                    rect
                        [ x -border
                        , y -border
                        , width (gameWidth + border * 2)
                        , height (gameHeight + border * 2)
                        , fill (Paint (colorFromHex "#234000"))
                        ]
                        []
                        :: (Types.allCharacters
                                |> shuffle generatedSeed
                                |> List.Extra.greedyGroupsOf perRow
                                |> List.indexedMap
                                    (\y row ->
                                        List.indexedMap
                                            (\x avatar ->
                                                g
                                                    [ transform
                                                        [ Scale scale scale
                                                        , Translate
                                                            (toFloat (x + (perRow - List.length row) // 2)
                                                                + ((gameWidth / scale - perRow) / 2)
                                                            )
                                                            (toFloat y + 0.75)
                                                        ]
                                                    , onClick (PickedAvatar avatar)
                                                    , cursor CursorPointer
                                                    ]
                                                    [ Avatars.characterToAvatar avatar
                                                        |> Avataaars.view
                                                            { width = 1
                                                            , height = 1
                                                            }
                                                    ]
                                            )
                                            row
                                    )
                                |> List.concat
                                |> (::)
                                    (centeredText
                                        [ x (gameWidth / 2)
                                        , y (scale / 2)
                                        , fill (Paint Color.white)
                                        ]
                                        [ text "Pick your avatar" ]
                                    )
                           )

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

                        currentPlay : Maybe OpposedHands
                        currentPlay =
                            case inGameModel.game of
                                PreparingHand _ _ ->
                                    Nothing

                                PlayedHand playerHand opponentHand ->
                                    Just ( playerHand, opponentHand )

                                GameFinished ->
                                    Nothing

                        specific : List (Svg Msg)
                        specific =
                            case inGameModel.game of
                                PreparingHand playerHand _ ->
                                    List.filterMap identity
                                        [ playHandButton playerHand ]

                                PlayedHand _ _ ->
                                    let
                                        lastHand : Bool
                                        lastHand =
                                            List.length inGameModel.playedHands
                                                == handCount

                                        label : String
                                        label =
                                            if lastHand then
                                                "Final score"

                                            else
                                                "Next hand"
                                    in
                                    [ bottomButton (GameMsg NextRound) label ]

                                GameFinished ->
                                    gameFinishedView inGameModel
                    in
                    [ backgroundRect
                    , g [] specific
                    , viewAvatar (Types.previous inGameModel.currentAvatar)
                    , g [ transform [ Translate 0 3 ] ] [ viewAvatar inGameModel.currentAvatar ]
                    , g [ transform [ Translate 6.3 0 ] ] (viewPreviousBest inGameModel.previousGames)
                    , g [ transform [ Translate 6.3 3 ] ] (viewPlayerScore inGameModel.playedHands currentPlay)
                    , g [ id "cards" ] (viewCards inGameModel)
                    ]
    in
    svg
        [ viewBox -border -border (gameWidth + border * 2) (gameHeight + border * 2)
        , strokeWidth 0.05
        , fontSize 0.25
        ]
        children


gameFinishedView : InGameModel -> List (Svg Msg)
gameFinishedView inGameModel =
    let
        finalPlayerScore : Float
        finalPlayerScore =
            playerGameScore inGameModel.playedHands
    in
    case inGameModel.previousGames of
        [] ->
            [ centeredText
                [ x 3.5
                , y 1.4
                , fill (Paint Color.white)
                ]
                [ tspan
                    [ x 3.5
                    , TypedSvg.Attributes.InEm.dy 1.2
                    ]
                    [ text
                        ("Your final score is {final}."
                            |> Format.float "final" finalPlayerScore
                        )
                    ]
                , tspan
                    [ x 3.5
                    , TypedSvg.Attributes.InEm.dy 1.2
                    ]
                    [ text "That's pretty bad," ]
                , tspan
                    [ x 3.5
                    , TypedSvg.Attributes.InEm.dy 1.2
                    ]
                    [ text "think you can do better?" ]
                ]
            , bottomButton (GameMsg NextLoop) "Next loop"
            ]

        previousGame :: tail ->
            let
                previousBest : Float
                previousBest =
                    playerGameScore previousGame
            in
            if previousBest < finalPlayerScore then
                [ [ "Your final score is {final}."
                        |> Format.float "final" finalPlayerScore
                  , "That's slightly better,"
                  , "think you can do more?"
                  ]
                    |> textBlock
                        { x = 3.5
                        , y = 1.4
                        , color = Color.white
                        }
                , bottomButton (GameMsg NextLoop) "Next loop"
                ]

            else
                [ ([ "Your final score is {final}."
                        |> Format.float "final" finalPlayerScore
                   , "That's {compare} than {previousBest}."
                        |> Format.string "compare"
                            (if previousBest == finalPlayerScore then
                                "not better"

                             else
                                "worse"
                            )
                        |> Format.float "previousBest" previousBest
                   ]
                    ++ (case tail of
                            [] ->
                                [ "You failed to improve"
                                , "your score at all."
                                ]

                            [ _ ] ->
                                [ "You managed to improve your"
                                , "score only once before failing."
                                ]

                            [ _, _ ] ->
                                [ "You managed to improve your"
                                , "score twice before failing."
                                ]

                            _ ->
                                [ "You managed to improve your"
                                , "score {len} times before failing."
                                    |> Format.int "len" (List.length tail)
                                ]
                       )
                  )
                    |> textBlock
                        { x = 3.5
                        , y = 1.25
                        , color = Color.white
                        }
                , bottomButton (GameMsg NextGame) "Try again"
                ]


textBlock : { x : Float, y : Float, color : Color } -> List String -> Svg Msg
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
            , fill (Paint attrs.color)
            ]


viewPreviousBest : List Game -> List (Svg msg)
viewPreviousBest previousGames =
    if List.isEmpty previousGames then
        []

    else
        [ centeredText
            [ x 0.5
            , y 0.45
            , fill (Paint Color.white)
            , dominantBaseline DominantBaselineAuto
            ]
            [ text "Previous" ]
        , centeredText
            [ x 0.5
            , y 0.55
            , fill (Paint Color.white)
            , dominantBaseline DominantBaselineHanging
            ]
            [ previousGames
                |> List.reverse
                |> List.map (\game -> game |> playerGameScore |> String.fromFloat)
                |> String.join " \u{00A0}"
                |> text
            ]
        ]


playerGameScore : Game -> Float
playerGameScore hands =
    hands
        |> List.map playerHandScore
        |> List.sum


playerHandScore : OpposedHands -> Float
playerHandScore ( playerHand, opponentHand ) =
    case CardSet.compare playerHand opponentHand of
        LT ->
            0

        EQ ->
            0.5

        GT ->
            1


viewPlayerScore : List OpposedHands -> Maybe OpposedHands -> List (Svg Msg)
viewPlayerScore playedHands play =
    let
        before : Float
        before =
            playerGameScore playedHands
    in
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
                text (String.fromFloat before)

            Just p ->
                let
                    playScore : Float
                    playScore =
                        playerHandScore p
                in
                if playScore == 0 then
                    text (String.fromFloat before)

                else
                    "{before} ⇒ {after}"
                        |> Format.float "before" before
                        |> Format.float "after" (before + playScore)
                        |> text
        ]
    ]


viewCards : InGameModel -> List (Svg Msg)
viewCards inGameModel =
    List.map (viewPlayerCard inGameModel) deck
        ++ List.map (viewOpponentCard inGameModel) deck


viewPlayerCard : InGameModel -> Card Player -> Svg Msg
viewPlayerCard inGameModel ((Card cardSuit cardValue) as card) =
    let
        discardPile : CardSet Player
        discardPile =
            List.foldl (\( p, _ ) a -> CardSet.union p a) CardSet.empty inGameModel.playedHands

        viewIfInDiscardPile : () -> Maybe (Svg msg)
        viewIfInDiscardPile =
            \_ ->
                CardSet.indexOf card discardPile
                    |> Maybe.map
                        (\index ->
                            viewCard []
                                { x = 6 -- + deckLerp index
                                , y = 2
                                , card = card
                                , cardState = FaceDown
                                }
                        )

        specific : List (() -> Maybe (Svg GameMsg))
        specific =
            case inGameModel.game of
                PreparingHand playerHand _ ->
                    let
                        viewIfSelected : () -> Maybe (Svg GameMsg)
                        viewIfSelected =
                            \_ ->
                                CardSet.indexOf card playerHand
                                    |> Maybe.map
                                        (\index ->
                                            viewCard
                                                [ onClick (Unselect card)
                                                , cursor CursorPointer
                                                ]
                                                { x = 2 + toFloat index * (cardWidth + 0.2)
                                                , y = 2
                                                , card = card
                                                , cardState = FaceUp
                                                }
                                        )

                        viewIfInDeck : () -> Maybe (Svg GameMsg)
                        viewIfInDeck =
                            \_ ->
                                let
                                    reducedHand : CardSet Player
                                    reducedHand =
                                        CardSet.diff
                                            (CardSet.diff (CardSet.fromList deck) discardPile)
                                            playerHand
                                in
                                if CardSet.member card reducedHand then
                                    viewCard
                                        [ onClick (Select card)
                                        , cursor CursorPointer
                                        ]
                                        { x = 1 + (14 - toFloat cardValue) * (cardWidth + 0.2)
                                        , y =
                                            case cardSuit of
                                                Hearts ->
                                                    4

                                                Bells ->
                                                    5

                                                Leaves ->
                                                    6

                                                Acorns ->
                                                    7
                                        , card = card
                                        , cardState = FaceUp
                                        }
                                        |> Just

                                else
                                    Nothing
                    in
                    [ viewIfSelected
                    , viewIfInDeck
                    ]

                PlayedHand playerHand opponentHand ->
                    let
                        viewIfSelected : () -> Maybe (Svg GameMsg)
                        viewIfSelected =
                            \_ ->
                                CardSet.indexOf card playerHand
                                    |> Maybe.map
                                        (\index ->
                                            viewCard
                                                [ onClick (Unselect card)
                                                , cursor CursorPointer
                                                ]
                                                { x = 2 + toFloat index * (cardWidth + 0.2)
                                                , y = 2
                                                , card = card
                                                , cardState =
                                                    case CardSet.compare playerHand opponentHand of
                                                        LT ->
                                                            FaceUp

                                                        EQ ->
                                                            FaceUp

                                                        GT ->
                                                            Desaturated
                                                }
                                        )

                        viewIfInDeck : () -> Maybe (Svg GameMsg)
                        viewIfInDeck =
                            \_ ->
                                let
                                    reducedHand : CardSet Player
                                    reducedHand =
                                        CardSet.diff
                                            (CardSet.diff (CardSet.fromList deck) discardPile)
                                            playerHand
                                in
                                if CardSet.member card reducedHand then
                                    viewCard
                                        [ onClick (Select card)
                                        , cursor CursorPointer
                                        ]
                                        { x = 1 + (14 - toFloat cardValue) * (cardWidth + 0.2)
                                        , y =
                                            case cardSuit of
                                                Hearts ->
                                                    3

                                                Bells ->
                                                    4

                                                Leaves ->
                                                    5

                                                Acorns ->
                                                    6
                                        , card = card
                                        , cardState = FaceUp
                                        }
                                        |> Just

                                else
                                    Nothing
                    in
                    [ viewIfSelected
                    , viewIfInDeck
                    ]

                GameFinished ->
                    []
    in
    findFirst (viewIfInDiscardPile :: specific)
        |> TypedSvg.Core.map GameMsg


findFirst :
    List
        (() -> Maybe (Svg msg))
    -> Svg msg
findFirst list =
    list
        |> List.Extra.findMap (\f -> f ())
        |> Maybe.withDefault (text "")


viewOpponentCard : InGameModel -> Card Opponent -> Svg Msg
viewOpponentCard inGameModel card =
    let
        discardPile : CardSet Opponent
        discardPile =
            List.foldl (\( _, o ) a -> CardSet.union o a) CardSet.empty inGameModel.playedHands

        viewIfInDiscardPile : () -> Maybe (Svg msg)
        viewIfInDiscardPile =
            \_ ->
                if CardSet.member card discardPile then
                    -- List.Extra.elemIndex card deck
                    --     |> Maybe.map
                    --         (\index ->
                    viewCard []
                        { x = 6 -- + deckLerp index
                        , y = 1
                        , card = card
                        , cardState = FaceDown
                        }
                        |> Just
                    -- )

                else
                    Nothing

        specific : List (() -> Maybe (Svg msg))
        specific =
            case inGameModel.game of
                PreparingHand _ opponentHand ->
                    [ \_ ->
                        CardSet.indexOf card opponentHand
                            |> Maybe.map
                                (\index ->
                                    viewCard []
                                        { x = 1 + toFloat index * (cardWidth + 0.2)
                                        , y = 0
                                        , card = card
                                        , cardState = FaceUp
                                        }
                                )
                    ]

                PlayedHand playerHand opponentHand ->
                    [ \_ ->
                        CardSet.indexOf card opponentHand
                            |> Maybe.map
                                (\index ->
                                    viewCard []
                                        { x = 2 + toFloat index * (cardWidth + 0.2)
                                        , y = 1
                                        , card = card
                                        , cardState =
                                            case CardSet.compare playerHand opponentHand of
                                                LT ->
                                                    FaceUp

                                                EQ ->
                                                    FaceUp

                                                GT ->
                                                    Desaturated
                                        }
                                )
                    ]

                GameFinished ->
                    []
    in
    findFirst (viewIfInDiscardPile :: specific)


deckLerp : Int -> Float
deckLerp index =
    (100 - toFloat index) / toFloat deckSize * (1 - cardWidth) - 1.1


playHandButton : CardSet Player -> Maybe (Svg Msg)
playHandButton playerHand =
    if CardSet.size playerHand == handSize then
        Just <| bottomButton (GameMsg SubmitHand) "Play hand"

    else
        Nothing


bottomButton : msg -> String -> Svg msg
bottomButton msg label =
    g
        [ transform [ Translate 1.1 3.1 ]
        , onClick msg
        , cursor CursorPointer
        ]
        [ rect
            [ x 0
            , y 0
            , width 5.2
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
            [ x (5.2 / 2)
            , y (cardHeight / 2)
            ]
            [ text label ]
        ]


shuffle : Random.Seed -> List a -> List a
shuffle seed list =
    Random.step
        (Random.List.shuffle list)
        seed
        |> Tuple.first


type CardState
    = FaceDown
    | FaceUp
    | Desaturated


viewCard :
    List (Attribute msg)
    ->
        { x : Float
        , y : Float
        , cardState : CardState
        , card : Card kind
        }
    -> Svg msg
viewCard attrs config =
    let
        (Card cardSuit cardValue) =
            config.card

        margin : Float
        margin =
            0.1

        cardRect : Svg msg
        cardRect =
            let
                border : Color
                border =
                    Color.charcoal

                cardBackground : Color
                cardBackground =
                    case config.cardState of
                        FaceUp ->
                            -- Oklch.toColor
                            --     { alpha = 1
                            --     , lightness = 0.85
                            --     , chroma = 0.1
                            --     , hue = (toFloat cardValue - 1) / toFloat deckSize
                            --     }
                            Color.white

                        Desaturated ->
                            -- Oklch.toColor
                            --     { alpha = 1
                            --     , lightness = 0.85
                            --     , chroma = 0.01
                            --     , hue = (toFloat cardValue - 1) / toFloat deckSize
                            --     }
                            Color.gray

                        FaceDown ->
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
        (cardRect
            :: (let
                    cardName : String
                    cardName =
                        cardValueToString cardValue ++ suitToString cardSuit

                    ( paint, label ) =
                        case config.cardState of
                            FaceUp ->
                                ( Color.black, [ text cardName ] )

                            Desaturated ->
                                ( Color.rgb 0.35 0.35 0.35, [ text cardName ] )

                            FaceDown ->
                                ( Color.green, [] )
                in
                [ centeredText
                    [ x (cardWidth / 2 + margin)
                    , y (cardHeight / 2 + margin)
                    , fill (Paint paint)
                    ]
                    label
                ]
               )
        )


cardValueToString : Int -> String
cardValueToString v =
    case v of
        14 ->
            "A"

        13 ->
            "K"

        12 ->
            "O"

        11 ->
            "U"

        _ ->
            String.fromInt v


suitToString : Suit -> String
suitToString suit =
    case suit of
        Hearts ->
            "♥️"

        Bells ->
            "🔔"

        Leaves ->
            "🌱"

        Acorns ->
            "🌰"


viewAvatar : Avatar -> Svg msg
viewAvatar character =
    Avataaars.view
        { width = 1
        , height = 1
        }
        (Avatars.characterToAvatar character)


subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none
