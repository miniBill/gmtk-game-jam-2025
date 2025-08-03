module CardSet exposing (CardSet, calculateHandKind, compare, diff, empty, forgetHandKind, fromList, giveToOpponent, handKind, indexOf, insert, member, remove, size, union)

import FastDict as Dict exposing (Dict)
import HandKind exposing (HandKind)
import Types exposing (Card(..), Opponent, Player, Suit(..))


suitToInt : Suit -> number
suitToInt suit =
    case suit of
        Hearts ->
            0

        Bells ->
            1

        Leaves ->
            2

        Acorns ->
            3


type CardSet kind handKind
    = CardSet (Dict ( Int, Int ) (Card kind)) (Dict Int (List (Card kind))) handKind


empty : CardSet kind ()
empty =
    CardSet Dict.empty Dict.empty ()


diff : CardSet kind () -> CardSet kind () -> CardSet kind ()
diff (CardSet a _ _) (CardSet b _ _) =
    build (Dict.diff a b)


union : CardSet kind () -> CardSet kind () -> CardSet kind ()
union (CardSet a _ _) (CardSet b _ _) =
    build (Dict.union a b)


build : Dict ( Int, Int ) (Card kind) -> CardSet kind ()
build d =
    CardSet d
        (Dict.foldl
            (\_ ((Card _ v) as c) a ->
                Dict.update v
                    (\e ->
                        e
                            |> Maybe.withDefault []
                            |> (::) c
                            |> Just
                    )
                    a
            )
            Dict.empty
            d
        )
        ()


fromList : List (Card kind) -> CardSet kind ()
fromList list =
    build (List.foldl (\e a -> Dict.insert (toKey e) e a) Dict.empty list)


toKey : Card kind -> ( Int, Int )
toKey (Card suit value) =
    ( value, suitToInt suit )


handKind : CardSet kind handKind -> handKind
handKind (CardSet _ _ k) =
    k


size : CardSet kind handKind -> Int
size (CardSet s _ _) =
    Dict.size s


insert : Card kind -> CardSet kind handKind -> CardSet kind ()
insert v (CardSet s _ _) =
    build (Dict.insert (toKey v) v s)


remove : Card kind -> CardSet kind handKind -> CardSet kind ()
remove v (CardSet s _ _) =
    build (Dict.remove (toKey v) s)


member : Card kind -> CardSet kind handKind -> Bool
member card (CardSet s _ _) =
    Dict.member (toKey card) s


indexOf : Card kind -> CardSet kind handKind -> Maybe Int
indexOf card ((CardSet s _ _) as set) =
    if member card set then
        Dict.foldr
            (\_ e ( a, i ) ->
                if e == card then
                    ( Just i, i + 1 )

                else
                    ( a, i + 1 )
            )
            ( Nothing, 0 )
            s
            |> Tuple.first

    else
        Nothing


compare : CardSet Player HandKind -> CardSet Opponent HandKind -> Order
compare playerHand opponentHand =
    HandKind.compare (handKind playerHand) (handKind opponentHand)


giveToOpponent : CardSet Player handKind -> CardSet Opponent handKind
giveToOpponent (CardSet s g k) =
    CardSet (Dict.map (always Types.giveToOpponent) s) (Dict.map (always (List.map Types.giveToOpponent)) g) k


calculateHandKind : CardSet kind () -> Maybe (CardSet kind HandKind)
calculateHandKind (CardSet s g _) =
    HandKind.calculate (Dict.values s)
        |> Maybe.map (CardSet s g)


forgetHandKind : CardSet kind HandKind -> CardSet kind ()
forgetHandKind (CardSet s g _) =
    CardSet s g ()
