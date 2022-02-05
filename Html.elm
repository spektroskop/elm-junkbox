module Junkbox.Html exposing
    ( maybe
    , maybeAttribute
    , none
    , unless
    , unlessLazy
    , when
    , whenAttribute
    , whenClass
    , whenLazy
    )

import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (class)


none : Html msg
none =
    text ""


whenAttribute : Bool -> Attribute msg -> Attribute msg
whenAttribute cond attr =
    if cond then
        attr

    else
        class ""


whenClass : Bool -> String -> Attribute msg
whenClass cond name =
    whenAttribute cond (class name)


maybeAttribute : Maybe a -> (a -> Attribute msg) -> Attribute msg
maybeAttribute m f =
    Maybe.map f m |> Maybe.withDefault (class "")


maybe : Maybe a -> (a -> Html msg) -> Html msg
maybe m f =
    Maybe.map f m |> Maybe.withDefault none


when : Bool -> Html msg -> Html msg
when cond v =
    if cond then
        v

    else
        none


whenLazy : Bool -> (() -> Html msg) -> Html msg
whenLazy cond value =
    if cond then
        value ()

    else
        none


unless : Bool -> Html msg -> Html msg
unless cond =
    when (not cond)


unlessLazy : Bool -> (() -> Html msg) -> Html msg
unlessLazy cond value =
    whenLazy (not cond) value
