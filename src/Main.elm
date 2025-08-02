module Main exposing (Model, Msg, main)

import Avataaars
import Avataaars.Graphics exposing (Graphics)
import Avatars
import Browser
import CardSet exposing (CardSet)
import Color exposing (Color)
import Color.Extra exposing (colorFromHex)
import Format
import HandKind
import List.Extra
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Random
import Random.List
import TypedSvg exposing (defs, filter, g, image, rect, svg, tspan)
import TypedSvg.Attributes as Attributes exposing (class, cursor, dominantBaseline, fill, href, id, stroke, style, transform, viewBox)
import TypedSvg.Attributes.InEm
import TypedSvg.Attributes.InPx exposing (fontSize, height, rx, strokeWidth, width, x, y)
import TypedSvg.Core exposing (Attribute, Svg, attribute, text)
import TypedSvg.Events exposing (onClick)
import TypedSvg.Extra exposing (centeredText)
import TypedSvg.Filters exposing (colorMatrix)
import TypedSvg.Filters.Attributes exposing (colorMatrixType, colorMatrixValues, in_)
import TypedSvg.Types exposing (ClipPath(..), ColorMatrixType(..), Cursor(..), DominantBaseline(..), Filter(..), InValue(..), Paint(..), Transform(..))
import Types exposing (Card(..), Character, Flags, Opponent, Player, Suit(..))


handSize : number
handSize =
    5


handCount : Int
handCount =
    List.length deck // handSize


deck : List (Card kind)
deck =
    List.Extra.lift2 Card allSuits (List.range 7 14)


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
                    opponentGameGenerator generatedSeed
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
                            opponentGameGenerator inGameModel.seed
                    in
                    ( initGame avatar newSeed opponentGame
                    , Cmd.none
                    )

                ( NextGame, _ ) ->
                    ( model, Cmd.none )

        ( GameMsg _, _ ) ->
            ( model, Cmd.none )


opponentGameGenerator : Random.Seed -> ( NonEmpty (CardSet Opponent), Random.Seed )
opponentGameGenerator seed =
    let
        go : List (CardSet Opponent) -> List (Card Opponent) -> ( NonEmpty (CardSet Opponent), Random.Seed )
        go acc queue =
            let
                groupsWith : number -> List a -> List (List a)
                groupsWith n l =
                    if n <= 0 then
                        l |> List.Extra.groupsOf 5

                    else
                        groupsWith (n - 1) (shuffle seed l)

                sorted : List ( HandKind.HandKind, List (Card Opponent) )
                sorted =
                    (groupsWith 0 queue ++ groupsWith 1 queue ++ groupsWith 2 queue)
                        |> List.map (\hand -> ( HandKind.calculate hand, hand ))
                        |> List.sortWith (\( k1, _ ) ( k2, _ ) -> HandKind.compare k1 k2)
            in
            case List.Extra.last sorted of
                Just ( _, firstHandList ) ->
                    let
                        firstHand : CardSet Opponent
                        firstHand =
                            CardSet.fromList firstHandList
                    in
                    go (firstHand :: acc) (List.Extra.removeWhen (\c -> CardSet.member c firstHand) queue)

                Nothing ->
                    case List.reverse acc of
                        [] ->
                            let
                                _ =
                                    Debug.todo "Wut"
                            in
                            ( ( CardSet.empty, [] ), seed )

                        head :: tail ->
                            Random.step (Random.constant ( head, tail )) seed
    in
    go [] deck


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
            7.4

        gameHeight : Float
        gameHeight =
            7

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
                    [ rect
                        [ x -border
                        , y -border
                        , width (gameWidth + border * 2)
                        , height (gameHeight + border * 2)
                        , fill (Paint (colorFromHex "#234000"))
                        ]
                        []
                    , Types.allCharacters
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
                        |> g []
                    , centeredText
                        [ x (gameWidth / 2)
                        , y (scale / 2)
                        , fill (Paint Color.white)
                        ]
                        [ text "Pick your avatar" ]
                    ]

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
                                    [ leftButton (GameMsg NextRound) label ]

                                GameFinished ->
                                    gameFinishedView inGameModel
                    in
                    [ defs []
                        [ TypedSvg.clipPath [ id "card-clip" ]
                            [ rect
                                [ y cardClipping.top
                                , x cardClipping.left
                                , width (cardWidth - cardClipping.width)
                                , height (cardHeight - cardClipping.height)
                                , rx cardClipping.rx
                                ]
                                []
                            ]
                        , filter [ id "desaturate" ]
                            [ colorMatrix
                                [ in_ InSourceGraphic
                                , colorMatrixType ColorMatrixTypeMatrix
                                , [ "0.2 0.2 0.2 0 0.1"
                                  , "0.2 0.2 0.2 0 0.1"
                                  , "0.2 0.2 0.2 0 0.1"
                                  , "0 0 0 1 0"
                                  ]
                                    |> String.join " "
                                    |> colorMatrixValues
                                ]
                                []
                            ]
                        ]
                    , backgroundRect
                    , g [ id "per-state" ] specific
                    , g [ id "opponent-avatar" ] [ viewAvatar (Types.previous inGameModel.currentAvatar) ]
                    , g [ id "avatar", transform [ Translate 0 3 ] ] [ viewAvatar inGameModel.currentAvatar ]
                    , g [ id "previous-best", transform [ Translate 6.3 1 ] ] (viewPreviousBest inGameModel.previousGames)
                    , g [ id "score", transform [ Translate 6.3 2 ] ] (viewPlayerScore inGameModel.playedHands currentPlay)
                    , g [ id "cards" ] (viewCards inGameModel)
                    ]
    in
    svg
        [ strokeWidth 0.05
        , fontSize 0.25
        , viewBox -border -border (gameWidth + border * 2) (gameHeight + border * 2)
        ]
        children


cardClipping : { left : Float, top : Float, width : Float, height : Float, rx : Float }
cardClipping =
    { left = 0.063
    , top = 0.036
    , width = 0.125
    , height = 0.075
    , rx = 0.05
    }


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
                    [ text "think you can beat that?" ]
                ]
            , leftButton (GameMsg NextLoop) "Next loop"
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
                  , "think you can beat yourself again?"
                  ]
                    |> textBlock
                        { x = 3.5
                        , y = 1.4
                        , color = Color.white
                        }
                , leftButton (GameMsg NextLoop) "Next loop"
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
                , leftButton (GameMsg NextGame) "Try again"
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
    case List.head previousGames of
        Nothing ->
            []

        Just lastGame ->
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
                [ lastGame
                    |> playerGameScore
                    |> String.fromFloat
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
        isPreparing : Bool
        isPreparing =
            case inGameModel.game of
                PreparingHand _ _ ->
                    True

                GameFinished ->
                    False

                PlayedHand _ _ ->
                    False

        ( playerHand, opponentHand ) =
            case inGameModel.game of
                PreparingHand ph oh ->
                    ( ph, oh )

                PlayedHand ph oh ->
                    ( ph, oh )

                GameFinished ->
                    ( CardSet.empty, CardSet.empty )

        discardPile : CardSet Player
        discardPile =
            List.foldl (\( p, _ ) a -> CardSet.union p a) CardSet.empty inGameModel.playedHands

        viewIfInDiscardPile : () -> Maybe (Svg msg)
        viewIfInDiscardPile =
            \_ ->
                if CardSet.member card discardPile then
                    viewCard
                        { x = 5.5
                        , y = 1
                        , card = card
                        , cardState = FaceDown
                        , onClick = Nothing
                        }
                        |> Just

                else
                    Nothing

        viewIfSelected : () -> Maybe (Svg GameMsg)
        viewIfSelected =
            \_ ->
                CardSet.indexOf card playerHand
                    |> Maybe.map
                        (\index ->
                            viewCard
                                { onClick = Just (Unselect card)
                                , x = 2 + toFloat index * (cardWidth + 0.2)
                                , y =
                                    if isPreparing then
                                        2

                                    else
                                        1
                                , card = card
                                , cardState =
                                    if isPreparing then
                                        if HandKind.belongs card (CardSet.handKind playerHand) then
                                            FaceUp

                                        else
                                            Desaturated

                                    else
                                        case CardSet.compare playerHand opponentHand of
                                            LT ->
                                                Desaturated

                                            EQ ->
                                                FaceUp

                                            GT ->
                                                FaceUp
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
                        { onClick =
                            if CardSet.size playerHand < handSize then
                                Just (Select card)

                            else
                                Nothing
                        , x = 1.1 + (14 - toFloat cardValue) * (cardWidth + 0.2)
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
                        , cardState =
                            if CardSet.size playerHand < handSize then
                                FaceUp

                            else
                                Desaturated
                        }
                        |> Just

                else
                    Nothing
    in
    findFirst
        [ viewIfInDiscardPile
        , viewIfSelected
        , viewIfInDeck
        ]
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
        isPreparing : Bool
        isPreparing =
            case inGameModel.game of
                PreparingHand _ _ ->
                    True

                GameFinished ->
                    False

                PlayedHand _ _ ->
                    False

        ( playerHand, opponentHand ) =
            case inGameModel.game of
                PreparingHand ph oh ->
                    ( ph, oh )

                PlayedHand ph oh ->
                    ( ph, oh )

                GameFinished ->
                    ( CardSet.empty, CardSet.empty )

        discardPile : CardSet Opponent
        discardPile =
            List.foldl (\( _, o ) a -> CardSet.union o a) CardSet.empty inGameModel.playedHands

        viewIfInDiscardPile : () -> Maybe (Svg msg)
        viewIfInDiscardPile =
            \_ ->
                if CardSet.member card discardPile then
                    viewCard
                        { x = 5.5
                        , y = 0
                        , card = card
                        , cardState = FaceDown
                        , onClick = Nothing
                        }
                        |> Just

                else
                    Nothing

        viewOtherwise : () -> Maybe (Svg msg)
        viewOtherwise =
            \_ ->
                viewCard
                    { x = 1.1
                    , y = 0
                    , card = card
                    , cardState = FaceDown
                    , onClick = Nothing
                    }
                    |> Just

        viewIfNextHand : () -> Maybe (Svg msg)
        viewIfNextHand =
            \_ ->
                CardSet.indexOf card opponentHand
                    |> Maybe.map
                        (\index ->
                            viewCard
                                { x = 2 + toFloat index * (cardWidth + 0.2)
                                , y = 0
                                , card = card
                                , cardState =
                                    if isPreparing then
                                        if HandKind.belongs card (CardSet.handKind opponentHand) then
                                            FaceUp

                                        else
                                            Desaturated

                                    else
                                        case CardSet.compare playerHand opponentHand of
                                            LT ->
                                                FaceUp

                                            EQ ->
                                                FaceUp

                                            GT ->
                                                Desaturated
                                , onClick = Nothing
                                }
                        )
    in
    findFirst [ viewIfInDiscardPile, viewIfNextHand, viewOtherwise ]


playHandButton : CardSet Player -> Maybe (Svg Msg)
playHandButton playerHand =
    if CardSet.size playerHand == handSize then
        Just <| leftButton (GameMsg SubmitHand) "Play hand"

    else
        Nothing


leftButton : msg -> String -> Svg msg
leftButton msg label =
    g
        [ transform [ Translate 0 2.1 ]
        , onClick msg
        , cursor CursorPointer
        ]
        [ rect
            [ width 1.8
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
            [ x 0.9
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
    { x : Float
    , y : Float
    , cardState : CardState
    , card : Card kind
    , onClick : Maybe msg
    }
    -> Svg msg
viewCard config =
    let
        (Card cardSuit cardValue) =
            config.card

        cardRect : Svg msg
        cardRect =
            rect
                [ x 0.05
                , y 0.025
                , width (cardWidth - 0.1)
                , height (cardHeight - 0.05)
                , rx 0.1
                , fill (Paint Color.darkGreen)
                , stroke (Paint Color.charcoal)
                ]
                []

        cardName : String
        cardName =
            cardValueToString cardValue ++ suitToEmoji cardSuit

        ( paint, label ) =
            case config.cardState of
                FaceUp ->
                    ( Color.black, [ text cardName ] )

                Desaturated ->
                    ( Color.rgb 0.35 0.35 0.35, [ text cardName ] )

                FaceDown ->
                    ( Color.green, [] )

        textRect : Svg msg
        textRect =
            rect
                [ x (cardWidth / 2 - 0.1)
                , y (cardHeight / 2 - 0.08)
                , width 0.2
                , height 0.18
                , fill (Paint Color.white)
                ]
                []

        cardText : Svg msg
        cardText =
            centeredText
                [ x (cardWidth / 2)
                , y (cardHeight / 2)
                , fill (Paint paint)
                , fontSize 0.1
                ]
                label

        attrs : List (Attribute msg)
        attrs =
            class [ "card" ]
                :: transform
                    [ Translate (config.x + 0.1 - cardClipping.left) (config.y + 0.1 - cardClipping.top)
                    , Scale (1 / (1 - cardClipping.width)) (1 / (1 - cardClipping.height))
                    ]
                :: style
                    (String.join "; "
                        [ "transition: all 0.4s ease-in-out"
                        , "filter: drop-shadow(0.01px 0.03px 0.02px rgb(0 0 0 / 0.4))"
                        ]
                    )
                :: (case config.onClick of
                        Nothing ->
                            []

                        Just msg ->
                            [ onClick msg
                            , cursor CursorPointer
                            ]
                   )

        cardImage : Svg msg
        cardImage =
            image
                [ width cardWidth
                , height cardHeight
                , Attributes.clipPath (ClipPathFunc "url(#card-clip)")
                , href ("public/" ++ cardSuitToString cardSuit ++ " " ++ cardValueToString cardValue ++ ".jpg")
                , stroke (Paint Color.charcoal)
                , if config.cardState == Desaturated then
                    Attributes.filter (Filter "url(#desaturate)")

                  else
                    attribute "data-empty" ""
                ]
                []
    in
    case config.cardState of
        FaceUp ->
            g
                attrs
                [ cardRect
                , cardImage
                , textRect
                , cardText
                ]

        Desaturated ->
            g
                attrs
                [ cardRect
                , cardImage
                , textRect
                , cardText
                ]

        FaceDown ->
            g
                attrs
                [ cardRect
                , text ""
                ]


cardSuitToString : Suit -> String
cardSuitToString suit =
    case suit of
        Hearts ->
            "Hearts"

        Bells ->
            "Bells"

        Leaves ->
            "Leaves"

        Acorns ->
            "Acorns"


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


suitToEmoji : Suit -> String
suitToEmoji suit =
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
