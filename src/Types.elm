module Types exposing (Card, Character(..), Flags, Opponent, Player, allCharacters, cardValue, characterToColor, giveToOpponent, isOpponentCard, next, opponentCard, playerCard, previous)

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
    = June
    | Rose
    | Dave
    | Jade
    | Aradia
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
    | Jane
    | Dirk
    | Roxy
    | Jake
    | Calliope
    | Caliborn


allCharacters : List ( Character, Graphics )
allCharacters =
    let
        go : ( Character, Graphics ) -> List ( Character, Graphics ) -> List ( Character, Graphics )
        go curr acc =
            case next curr of
                ( June, _ ) ->
                    List.reverse (curr :: acc)

                n ->
                    go n (curr :: acc)
    in
    go ( June, Bat ) []


characterToColor : Character -> String
characterToColor color =
    case color of
        June ->
            "#0715cd"

        Rose ->
            "#b536da"

        Dave ->
            "#e00707"

        Jade ->
            "#4ac925"

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

        Jane ->
            "#00d5f2"

        Dirk ->
            "#f2a400"

        Roxy ->
            "#ff6ff2"

        Jake ->
            "#1f9400"

        Calliope ->
            "#929292"

        Caliborn ->
            "#323232"


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
nextCharacter character =
    case character of
        June ->
            Rose

        Rose ->
            Dave

        Dave ->
            Jade

        Jade ->
            Karkat

        Aradia ->
            Nepeta

        Tavros ->
            Vriska

        Sollux ->
            Jane

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

        Jane ->
            Dirk

        Dirk ->
            Roxy

        Roxy ->
            Jake

        Jake ->
            Calliope

        Calliope ->
            Caliborn

        Caliborn ->
            June


previousCharacter : Character -> Character
previousCharacter character =
    let
        go : Character -> Character
        go candidate =
            if nextCharacter candidate == character then
                candidate

            else
                go (nextCharacter candidate)
    in
    go character


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
