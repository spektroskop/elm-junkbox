module Junkbox.Result exposing
    ( error
    , ok
    )


ok : Result e a -> Maybe a
ok result =
    case result of
        Ok value ->
            Just value

        Err _ ->
            Nothing


error : Result e a -> Maybe e
error result =
    case result of
        Err err ->
            Just err

        Ok _ ->
            Nothing
