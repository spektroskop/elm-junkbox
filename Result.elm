module Junkbox.Result exposing
    ( either
    , error
    , join
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


either : (x -> b) -> (a -> b) -> Result x a -> b
either onErr onOk result =
    case result of
        Err x ->
            onErr x

        Ok a ->
            onOk a


join : Result x (Result x a) -> Result x a
join result =
    case result of
        Err x ->
            Err x

        Ok (Err x) ->
            Err x

        Ok (Ok a) ->
            Ok a
