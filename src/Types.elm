module Types exposing (Color(..), Flags, colorToString, nextColor, nextGraphics, previousColor, previousGraphics)

import Avataaars.Graphics exposing (Graphics(..))


type alias Flags =
    {}


type Color
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


colorToString : Color -> String
colorToString color =
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


nextColor : Color -> Color
nextColor color =
    case color of
        Aradia ->
            Kanaya

        Tavros ->
            Terezi

        Sollux ->
            Vriska

        Karkat ->
            Equius

        Nepeta ->
            Gamzee

        Kanaya ->
            Eridan

        Terezi ->
            Feferi

        Vriska ->
            Aradia

        Equius ->
            Tavros

        Gamzee ->
            Sollux

        Eridan ->
            Karkat

        Feferi ->
            Nepeta


previousColor : Color -> Color
previousColor color =
    case color of
        Kanaya ->
            Aradia

        Terezi ->
            Tavros

        Vriska ->
            Sollux

        Equius ->
            Karkat

        Gamzee ->
            Nepeta

        Eridan ->
            Kanaya

        Feferi ->
            Terezi

        Aradia ->
            Vriska

        Tavros ->
            Equius

        Sollux ->
            Gamzee

        Karkat ->
            Eridan

        Nepeta ->
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
