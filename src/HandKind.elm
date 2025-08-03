module HandKind exposing (HandKind(..), belongs, calculate, compare, toString)

import Dict
import Dict.Extra
import Types exposing (Card(..))


type HandKind
    = HighCard Int
    | Pair Int
    | TwoPair Int Int
    | ThreeOfAKind Int
    | Straight Int
    | Flush Int
    | FullHouse Int Int
    | FourOfAKind Int
    | StraightFlush Int


calculate : List (Card kind) -> Maybe HandKind
calculate list =
    if List.length list /= 5 then
        Nothing

    else
        let
            sorted : List (Card kind)
            sorted =
                List.sortBy (\(Card _ v) -> v) list

            isFlush : Maybe HandKind
            isFlush =
                case list of
                    [] ->
                        Nothing

                    (Card s v) :: tail ->
                        if List.all (\(Card s2 _) -> s2 == s) tail then
                            Just (Flush (List.foldl (\(Card _ v2) a -> max a v2) v tail))

                        else
                            Nothing

            grouped : List ( Int, Int )
            grouped =
                sorted
                    |> List.map (\(Card _ v) -> v)
                    |> Dict.Extra.groupBy identity
                    |> Dict.map (\_ l -> List.length l)
                    |> Dict.toList
                    |> List.sortBy (\( f, s ) -> ( -s, -f ))
        in
        case grouped of
            ( v, 4 ) :: _ ->
                FourOfAKind v
                    |> Just

            [ ( v1, 3 ), ( v2, 2 ) ] ->
                FullHouse v1 v2
                    |> Just

            ( v1, 3 ) :: _ ->
                isFlush
                    |> Maybe.withDefault (ThreeOfAKind v1)
                    |> Just

            ( v1, 2 ) :: ( v2, 2 ) :: _ ->
                isFlush
                    |> Maybe.withDefault (TwoPair v1 v2)
                    |> Just

            ( v1, 2 ) :: _ ->
                isFlush
                    |> Maybe.withDefault (Pair v1)
                    |> Just

            [ ( v5, 1 ), ( v4, 1 ), ( v3, 1 ), ( v2, 1 ), ( v1, 1 ) ] ->
                if v2 == v1 + 1 && v3 == v2 + 1 && v4 == v3 + 1 && v5 == v4 + 1 then
                    case isFlush of
                        Just _ ->
                            StraightFlush v5 |> Just

                        Nothing ->
                            Straight v5 |> Just

                else
                    isFlush
                        |> Maybe.withDefault (HighCard v5)
                        |> Just

            ( v5, 1 ) :: _ ->
                isFlush
                    |> Maybe.withDefault (HighCard v5)
                    |> Just

            _ ->
                let
                    _ =
                        Debug.log "impossible case" grouped
                in
                isFlush


compare : HandKind -> HandKind -> Order
compare l r =
    if l == r then
        EQ

    else
        case ( l, r ) of
            ( HighCard lh, HighCard rh ) ->
                Basics.compare lh rh

            ( HighCard _, _ ) ->
                LT

            ( _, HighCard _ ) ->
                GT

            ( Pair lp, Pair rp ) ->
                Basics.compare lp rp

            ( Pair _, _ ) ->
                LT

            ( _, Pair _ ) ->
                GT

            ( TwoPair lh ll, TwoPair rh rl ) ->
                if lh == rh then
                    Basics.compare ll rl

                else
                    Basics.compare lh rh

            ( TwoPair _ _, _ ) ->
                LT

            ( _, TwoPair _ _ ) ->
                GT

            ( ThreeOfAKind lt, ThreeOfAKind rt ) ->
                Basics.compare lt rt

            ( ThreeOfAKind _, _ ) ->
                LT

            ( _, ThreeOfAKind _ ) ->
                GT

            ( Straight lh, Straight rh ) ->
                Basics.compare lh rh

            ( Straight _, _ ) ->
                LT

            ( _, Straight _ ) ->
                GT

            ( Flush lh, Flush rh ) ->
                Basics.compare lh rh

            ( Flush _, _ ) ->
                LT

            ( _, Flush _ ) ->
                GT

            ( FullHouse lh ll, FullHouse rh rl ) ->
                if lh == rh then
                    Basics.compare ll rl

                else
                    Basics.compare lh rh

            ( FullHouse _ _, _ ) ->
                LT

            ( _, FullHouse _ _ ) ->
                GT

            ( FourOfAKind lt, FourOfAKind rt ) ->
                Basics.compare lt rt

            ( FourOfAKind _, _ ) ->
                LT

            ( _, FourOfAKind _ ) ->
                GT

            ( StraightFlush lh, StraightFlush rh ) ->
                Basics.compare lh rh


belongs : Card kind -> HandKind -> Bool
belongs (Card _ v) kind =
    case kind of
        HighCard h ->
            h == v

        Pair h ->
            h == v

        ThreeOfAKind h ->
            h == v

        FourOfAKind h ->
            h == v

        TwoPair h l ->
            h == v || l == v

        Straight _ ->
            True

        Flush _ ->
            True

        FullHouse _ _ ->
            True

        StraightFlush _ ->
            True


toString : HandKind -> String
toString kind =
    case kind of
        HighCard v ->
            "High card, " ++ Types.cardValueToString v

        Pair v ->
            "One pair, " ++ Types.cardValueToString v ++ "s"

        TwoPair h l ->
            "Two pairs, " ++ Types.cardValueToString h ++ "s and " ++ Types.cardValueToString l ++ "s"

        ThreeOfAKind v ->
            "Three of a kind, " ++ Types.cardValueToString v ++ "s"

        Straight h ->
            Types.cardValueToString h ++ "-high straight"

        Flush h ->
            Types.cardValueToString h ++ "-high flush"

        FullHouse h l ->
            "Full house, " ++ Types.cardValueToString h ++ "s over " ++ Types.cardValueToString l ++ "s"

        FourOfAKind v ->
            "Four of a kind, " ++ Types.cardValueToString v ++ "s"

        StraightFlush h ->
            Types.cardValueToString h ++ "-high straight flush"
