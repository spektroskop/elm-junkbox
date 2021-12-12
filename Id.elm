module Junkbox.Id exposing
    ( Id(..)
    , Record
    , WithId
    , decoder
    , equal
    , fromString
    , toString
    )

import Json.Decode as Decode exposing (Decoder)
import Junkbox.Decode as Decode


type alias Record x =
    { id : Id x }


type alias WithId r x =
    { r | id : Id x }


type Id x
    = Id String


decoder : String -> Decoder (Id x)
decoder onError =
    Decode.requiredString onError |> Decode.map fromString


fromString : String -> Id x
fromString =
    Id


toString : Id x -> String
toString (Id id) =
    id


equal : Id a -> Id b -> Bool
equal (Id a) (Id b) =
    a == b
