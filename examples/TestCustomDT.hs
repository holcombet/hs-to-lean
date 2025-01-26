module TestCustomDT where 


-- Defining a custom data type for a simple shape
data Shape = Circle Float | Rectangle Float Float

-- A function to calculate the area of a shape
area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle l w) = l * w


data Tree = Nil | Node Int Tree Tree

insert :: Int -> Tree -> Tree
insert x Nil = Node x Nil Nil
insert x (Node a left right) = 
    if x < a then Node a (insert x left) right else Node a left (insert x right)

heapify :: [Int] -> Tree
heapify [] = Nil
heapify (x:xs) = insert x (heapify xs)

tree2list :: Tree -> [Int]
tree2list Nil = []
tree2list (Node a left right) = tree2list left ++ [a] ++ tree2list right

heapSort = tree2list . heapify 