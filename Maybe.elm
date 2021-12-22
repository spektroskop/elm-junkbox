module Junkbox.Maybe exposing
    ( firstOf
    , join
    , or
    , withDefaultLazy
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


withDefaultLazy : (() -> a) -> Maybe a -> a
withDefaultLazy d m =
    case m of
        Nothing ->
            d ()

        Just a ->
            a


join : Maybe (Maybe a) -> Maybe a
join maybe =
    case maybe of
        Nothing ->
            Nothing

        Just Nothing ->
            Nothing

        Just (Just a) ->
            Just a
