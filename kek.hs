module Kek where

data Tree a = Tr a (Tree a) (Tree a) | Nil

height:: Tree a -> Int
height Nil = 0
height (Tr _ left right) = 1 + max (height left) (height right)