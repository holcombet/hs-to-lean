module TestInsertionSort where

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) = if x < y 
                 then x:y:ys 
         else y : insert x ys

insertionSort :: [Int] -> [Int]
insertionSort [x] = [x]
insertionSort (x:xs) = insert x (insertionSort xs)