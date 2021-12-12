module Junkbox.RoseTree.Zipper exposing
    ( Zipper
    , firstChild
    , focus
    , lastChild
    , lastDescendant
    , mapFocus
    , mapNode
    , mapTree
    , next
    , nextSibling
    , nextTree
    , node
    , parent
    , previousSibling
    , root
    , tree
    , zipper
    )

import Junkbox.Maybe as Maybe
import Junkbox.RoseTree as Tree exposing (Tree)


type alias Crumb a =
    { node : a
    , before : List (Tree a)
    , after : List (Tree a)
    }


type Zipper a
    = Zipper
        { focus : Tree a
        , before : List (Tree a)
        , after : List (Tree a)
        , crumbs : List (Crumb a)
        }


reconstruct : Tree a -> List (Tree a) -> List (Tree a) -> a -> Tree a
reconstruct f b a n =
    Tree.tree n <|
        List.reverse b
            ++ [ f ]
            ++ a


zipper : Tree a -> Zipper a
zipper t =
    Zipper
        { focus = t
        , before = []
        , after = []
        , crumbs = []
        }


focus : Zipper a -> Tree a
focus (Zipper z) =
    z.focus


tree : Zipper a -> Tree a
tree =
    root >> focus


node : Zipper a -> a
node =
    focus >> Tree.node


root : Zipper a -> Zipper a
root z =
    case parent z of
        Nothing ->
            firstSibling z

        Just p ->
            root p


next : Zipper a -> Maybe (Zipper a)
next =
    Maybe.firstOf
        [ firstChild
        , nextSibling
        , nextAncestorSibling
        ]


nextTree : Zipper a -> Maybe (Zipper a)
nextTree =
    Maybe.firstOf
        [ nextSibling
        , nextAncestorSibling
        ]


parent : Zipper a -> Maybe (Zipper a)
parent (Zipper z) =
    case z.crumbs of
        [] ->
            Nothing

        x :: xs ->
            Just <|
                Zipper
                    { focus = reconstruct z.focus z.before z.after x.node
                    , before = x.before
                    , after = x.after
                    , crumbs = xs
                    }


firstChild : Zipper a -> Maybe (Zipper a)
firstChild (Zipper z) =
    case Tree.children z.focus of
        [] ->
            Nothing

        x :: xs ->
            let
                crumb =
                    { node = Tree.node z.focus
                    , before = z.before
                    , after = z.after
                    }
            in
            Just <|
                Zipper
                    { focus = x
                    , before = []
                    , after = xs
                    , crumbs = crumb :: z.crumbs
                    }


lastChild : Zipper a -> Maybe (Zipper a)
lastChild (Zipper z) =
    case List.reverse (Tree.children z.focus) of
        [] ->
            Nothing

        x :: xs ->
            let
                crumb =
                    { node = Tree.node z.focus
                    , before = z.before
                    , after = z.after
                    }
            in
            Just <|
                Zipper
                    { focus = x
                    , before = xs
                    , after = []
                    , crumbs = crumb :: z.crumbs
                    }


lastDescendant : Zipper a -> Zipper a
lastDescendant z =
    case lastChild z of
        Nothing ->
            z

        Just c ->
            lastDescendant c


firstSibling : Zipper a -> Zipper a
firstSibling z =
    case previousSibling z of
        Nothing ->
            z

        Just s ->
            firstSibling s


nextSibling : Zipper a -> Maybe (Zipper a)
nextSibling (Zipper z) =
    case z.after of
        [] ->
            Nothing

        x :: xs ->
            Just <|
                Zipper
                    { focus = x
                    , before = z.focus :: z.before
                    , after = xs
                    , crumbs = z.crumbs
                    }


previousSibling : Zipper a -> Maybe (Zipper a)
previousSibling (Zipper z) =
    case z.before of
        [] ->
            Nothing

        x :: xs ->
            Just <|
                Zipper
                    { focus = x
                    , before = xs
                    , after = z.focus :: z.after
                    , crumbs = z.crumbs
                    }


nextAncestorSibling : Zipper a -> Maybe (Zipper a)
nextAncestorSibling z =
    case parent z of
        Nothing ->
            Nothing

        Just p ->
            case nextSibling p of
                Nothing ->
                    nextAncestorSibling p

                Just s ->
                    Just s


mapFocus : (Tree a -> Tree a) -> Zipper a -> Zipper a
mapFocus f (Zipper z) =
    Zipper { z | focus = f z.focus }


mapTree : (a -> a) -> Zipper a -> Zipper a
mapTree f (Zipper z) =
    let
        crumbs =
            List.map (\c -> { c | node = f c.node }) z.crumbs
    in
    Zipper { z | crumbs = crumbs }
        |> mapFocus (Tree.map f)


mapNode : (a -> a) -> Zipper a -> Zipper a
mapNode =
    Tree.mapNode >> mapFocus
