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

main = do
    print $ heapSort [5,2,8,1,3,7,9]
    print $ heapSort [9,7,3,1,8,2,5]