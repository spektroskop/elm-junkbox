module Junkbox.Basics exposing
    ( after
    , flip
    , nonEmptyString
    , perform
    , uncurry
    )

import Process
import Task


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


flip : (a -> b -> c) -> (b -> a -> c)
flip f b a =
    f a b


after : Float -> msg -> Cmd msg
after sleep msg =
    Process.sleep sleep |> Task.perform (always msg)


perform : msg -> Cmd msg
perform msg =
    Task.succeed msg |> Task.perform identity


nonEmptyString : String -> Maybe String
nonEmptyString value =
    let
        trimmed =
            String.trim value
    in
    if String.isEmpty trimmed then
        Nothing

    else
        Just trimmed
