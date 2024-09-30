module Test (recursiveAdd) where

recursiveAdd :: Int -> Int -> Int
recursiveAdd n 0 = n
recursiveAdd n m = 1 + recursiveAdd n (m-1)


addOne :: Int -> Int
addOne n = n + 1


fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

