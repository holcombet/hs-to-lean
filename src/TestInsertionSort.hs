module TestInsertionSort where

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) = if x < y 
                 then x:y:ys 
         else y : insert x ys

insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

-- insertionSort2 :: [Int] -> [Int]
-- insertionSort2 xs = foldr insert [] xs

main = print $ insertionSort [5,1,4,2,9]