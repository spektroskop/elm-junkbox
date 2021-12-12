module Junkbox.RoseTree exposing
    ( Tree
    , appendChild
    , children
    , flatten
    , map
    , mapNode
    , node
    , singleton
    , tree
    )


type Tree a
    = Tree a (List (Tree a))


tree : a -> List (Tree a) -> Tree a
tree =
    Tree


singleton : a -> Tree a
singleton v =
    tree v []


node : Tree a -> a
node (Tree n _) =
    n


children : Tree a -> List (Tree a)
children (Tree _ cs) =
    cs


map : (a -> b) -> Tree a -> Tree b
map f (Tree n cs) =
    Tree (f n) (List.map (map f) cs)


mapNode : (a -> a) -> Tree a -> Tree a
mapNode f (Tree n cs) =
    Tree (f n) cs


appendChild : Tree a -> Tree a -> Tree a
appendChild c (Tree n cs) =
    Tree n (cs ++ [ c ])


flatten : Tree a -> List a
flatten (Tree n cs) =
    n :: List.concatMap flatten cs
