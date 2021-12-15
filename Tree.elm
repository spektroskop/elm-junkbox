module Junkbox.Tree exposing
    ( Tree
    , appendChild
    , children
    , flatten
    , fold
    , map
    , mapNode
    , node
    , singleton
    , sortWith
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


fold : (a -> r -> r) -> r -> Tree a -> r
fold f x (Tree n cs) =
    List.foldl (\t r -> fold f r t) (f n x) cs


sortWith : (a -> a -> Order) -> Tree a -> Tree a
sortWith comparator (Tree n cs) =
    List.sortWith (\a b -> comparator (node a) (node b)) cs
        |> List.map (sortWith comparator)
        |> Tree n
