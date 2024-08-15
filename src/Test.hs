module Test (recursiveAdd) where

recursiveAdd :: Int -> Int -> Int
recursiveAdd n 0 = n
recursiveAdd n m = 1 + recursiveAdd n (m-1)




