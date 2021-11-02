module Tree exposing (..)

type Tree a
    = Node a (List (Tree a))

node =
    Node

children (Node node children) =
    children
