module Junkbox.Decode exposing
    ( nonEmptyList
    , optionalString
    , requiredString
    )

import Json.Decode as Decode exposing (Decoder)
import Junkbox.Basics as Basics


maybe : String -> Maybe a -> Decoder a
maybe onError =
    Maybe.map Decode.succeed >> Maybe.withDefault (Decode.fail onError)


optionalString : Decoder (Maybe String)
optionalString =
    Decode.map Basics.nonEmptyString Decode.string


requiredString : String -> Decoder String
requiredString onError =
    optionalString |> Decode.andThen (maybe onError)


nonEmptyList : String -> Decoder a -> Decoder (List a)
nonEmptyList onError decoder =
    Decode.list decoder
        |> Decode.andThen
            (\items ->
                if List.isEmpty items then
                    Decode.fail onError

                else
                    Decode.succeed items
            )
