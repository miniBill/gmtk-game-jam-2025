module Types exposing (Card, Character(..), Flags, Opponent, Player, cardValue, characterToColor, giveToOpponent, isOpponentCard, next, opponentCard, playerCard, previous)

import Avataaars.Graphics exposing (Graphics(..))


type alias Flags =
    {}


playerCard : Int -> Card Player
playerCard c =
    Card { opponent = False } c


opponentCard : Int -> Card Opponent
opponentCard c =
    Card { opponent = True } c


cardValue : Card kind -> Int
cardValue (Card _ c) =
    c


isOpponentCard : Card kind -> Bool
isOpponentCard (Card { opponent } _) =
    opponent


type Card kind
    = Card { opponent : Bool } Int


type Player
    = Player


type Opponent
    = Opponent


type Character
    = Aradia
    | Tavros
    | Sollux
    | Karkat
    | Nepeta
    | Kanaya
    | Terezi
    | Vriska
    | Equius
    | Gamzee
    | Eridan
    | Feferi


characterToColor : Character -> String
characterToColor color =
    case color of
        Aradia ->
            "#a10000"

        Tavros ->
            "#a15000"

        Sollux ->
            "#a1a100"

        Karkat ->
            "#626262"

        Nepeta ->
            "#416600"

        Kanaya ->
            "#008141"

        Terezi ->
            "#008282"

        Vriska ->
            "#005682"

        Equius ->
            "#000056"

        Gamzee ->
            "#2b0057"

        Eridan ->
            "#6a006a"

        Feferi ->
            "#77003c"


next : ( Character, Graphics ) -> ( Character, Graphics )
next ( color, graphics ) =
    ( nextCharacter color
    , nextGraphics graphics
    )


previous : ( Character, Graphics ) -> ( Character, Graphics )
previous ( color, graphics ) =
    ( previousCharacter color
    , previousGraphics graphics
    )


nextCharacter : Character -> Character
nextCharacter color =
    case color of
        Aradia ->
            Nepeta

        Tavros ->
            Vriska

        Sollux ->
            Karkat

        Karkat ->
            Terezi

        Nepeta ->
            Eridan

        Kanaya ->
            Equius

        Terezi ->
            Gamzee

        Vriska ->
            Kanaya

        Equius ->
            Aradia

        Gamzee ->
            Tavros

        Eridan ->
            Feferi

        Feferi ->
            Sollux


previousCharacter : Character -> Character
previousCharacter color =
    case color of
        Nepeta ->
            Aradia

        Vriska ->
            Tavros

        Karkat ->
            Sollux

        Terezi ->
            Karkat

        Eridan ->
            Nepeta

        Equius ->
            Kanaya

        Gamzee ->
            Terezi

        Kanaya ->
            Vriska

        Aradia ->
            Equius

        Tavros ->
            Gamzee

        Feferi ->
            Eridan

        Sollux ->
            Feferi


nextGraphics : Graphics -> Graphics
nextGraphics graphics =
    case graphics of
        Bat ->
            Cumbia

        Cumbia ->
            Deer

        Deer ->
            Diamond

        Diamond ->
            SkullOutline

        SkullOutline ->
            Hola

        Hola ->
            Pizza

        Pizza ->
            Resist

        Resist ->
            Selena

        Selena ->
            Bear

        Bear ->
            Skull

        Skull ->
            Bat


previousGraphics : Graphics -> Graphics
previousGraphics graphics =
    case graphics of
        Bat ->
            Skull

        Cumbia ->
            Bat

        Deer ->
            Cumbia

        Diamond ->
            Deer

        SkullOutline ->
            Diamond

        Hola ->
            SkullOutline

        Pizza ->
            Hola

        Resist ->
            Pizza

        Selena ->
            Resist

        Bear ->
            Selena

        Skull ->
            Bear


giveToOpponent : Card Player -> Card Opponent
giveToOpponent (Card _ c) =
    Card { opponent = True } c
