module List.MyExtra exposing (findMapWithIndex)


findMapWithIndex : (number -> a -> Maybe b) -> List a -> Maybe b
findMapWithIndex f list =
    let
        go : number -> List a -> Maybe b
        go i queue =
            case queue of
                [] ->
                    Nothing

                head :: tail ->
                    case f i head of
                        Just res ->
                            Just res

                        Nothing ->
                            go (i + 1) tail
    in
    go 0 list
