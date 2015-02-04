module Weekfourhomework where

------------------------------------
-- Exercise 1: Wholemeal programming
------------------------------------

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate logic
    where logic n = if even n then n `div` 2 else 3 * n + 1

---------------------------------
-- Exercise 2: Folding with trees
---------------------------------

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x t@(Node h l y r) = Node (if hasSpace t then h else h + 1) r y (insert x l)

hasSpace :: Tree a -> Bool
hasSpace Leaf = False
hasSpace (Node _ Leaf _ Leaf) = False
hasSpace (Node _ l@Node{} _ r@Node{}) = hasSpace l || hasSpace r
hasSpace _ = True
