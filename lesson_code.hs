module Lucker where

--код в ккомментариях написан не мной
--data Tree a = Empty | Node (Tree a) a (Tree a)
{-
insert:: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x (Node left val right)
	| x < val = Node (insert x left) val rght
	| otherwise = Node left val (insert x right)

-}
data Tree a = Tr a (Tree a) (Tree a) | Nil
	deriving (Show)

height:: Tree a -> Int
height Nil = 0
height (Tr _ left right) = 1 + max (height left) (height right)


{-Task1: search tree insert-}
insert::(Ord a) => a -> Tree a -> Tree a
insert var Nil = Tr var Nil Nil
insert var (Tr el left Nil)
	|(var >= el) = Tr el left (Tr var Nil Nil)
	|(var < el) = Tr el (insert var left) Nil
insert var (Tr el Nil right)
	|(var >= el) = Tr el Nil (insert var right)
	|(var < el) = Tr el (Tr var Nil Nil) right
insert var (Tr el Nil Nil)
	|(var >= el) = Tr el Nil (Tr var Nil Nil)
	|(var < el) = Tr el (Tr var Nil Nil) Nil
insert var (Tr el left right) 
	|(var >= el) = insert var right
	|otherwise = insert var left

{-Task2: built tree-}
build::(Ord a) => [a] -> Tree a
build list
	| null list = error "Empty Tree"
	| otherwise = build' list Nil

build'::(Ord a) => [a] -> Tree a -> Tree a
build' (x:xs) prev
	|null xs = insert x prev
	|otherwise = build' xs (insert x prev) 
{-сделать обход дерева (RNL) || (LNR) || (NLR) || (NRL) e.t.c.-}
--traverse::Tree a -> [a]

--tfoldr дописать