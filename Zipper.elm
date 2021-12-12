module Junkbox.Zipper exposing
    ( Zipper(..)
    , focus
    , focusFirst
    , focusNextMatch
    , length
    , map
    , mapEach
    , mapFocus
    , new
    , next
    , prev
    , toList
    )


type Zipper a
    = Zipper (List a) a (List a)


new : a -> List a -> Zipper a
new =
    Zipper []


length : Zipper a -> Int
length (Zipper b _ a) =
    1 + List.length b + List.length a


focus : Zipper a -> a
focus (Zipper _ f _) =
    f


map : (a -> b) -> Zipper a -> Zipper b
map fun (Zipper b f a) =
    Zipper (List.map fun b) (fun f) (List.map fun a)


mapFocus : (a -> a) -> Zipper a -> Zipper a
mapFocus fun (Zipper b f a) =
    Zipper b (fun f) a


mapEach : (a -> b) -> (a -> b) -> (a -> b) -> Zipper a -> Zipper b
mapEach fb ff fa (Zipper b f a) =
    Zipper (List.map fb b) (ff f) (List.map fa a)


toList : Zipper a -> List a
toList (Zipper b f a) =
    List.concat
        [ List.reverse b
        , [ f ]
        , a
        ]


next : Zipper a -> Maybe (Zipper a)
next (Zipper b f a) =
    case a of
        [] ->
            Nothing

        v :: rest ->
            Zipper (f :: b) v rest
                |> Just



{- TODO: check -}


prev : Zipper a -> Maybe (Zipper a)
prev (Zipper b f a) =
    case b of
        [] ->
            Nothing

        v :: rest ->
            Zipper rest v (f :: a)
                |> Just


focusFirst : Zipper a -> Zipper a
focusFirst (Zipper b f a) =
    case b of
        [] ->
            Zipper b f a

        v :: rest ->
            Zipper [] v (f :: rest)


focusNextMatch : (a -> Bool) -> Zipper a -> Zipper a
focusNextMatch match zipper =
    if match (focus zipper) then
        zipper

    else
        Maybe.map (focusNextMatch match) (next zipper)
            |> Maybe.withDefault zipper
