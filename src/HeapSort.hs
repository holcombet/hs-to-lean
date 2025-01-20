
module HeapSort where 

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
