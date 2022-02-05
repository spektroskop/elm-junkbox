module Junkbox.List exposing
    ( Next(..)
    , foldUntil
    )


type Next a
    = Continue a
    | Stop a


foldUntil :
    (a -> acc -> Next acc)
    -> acc
    -> List a
    -> acc
foldUntil with acc from =
    case from of
        [] ->
            acc

        first :: rest ->
            case with first acc of
                Continue next ->
                    foldUntil with next rest

                Stop result ->
                    result
