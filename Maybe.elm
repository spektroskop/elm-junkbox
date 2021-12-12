module Junkbox.Maybe exposing
    ( firstOf
    , or
    )


or : Maybe a -> Maybe a -> Maybe a
or a b =
    case a of
        Nothing ->
            b

        Just _ ->
            a


firstOf : List (a -> Maybe b) -> a -> Maybe b
firstOf options v =
    case options of
        [] ->
            Nothing

        x :: xs ->
            case x v of
                Nothing ->
                    firstOf xs v

                Just r ->
                    Just r
