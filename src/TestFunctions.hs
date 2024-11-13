
module TestFunctions where

-- Pattern Matching Examples to Translate to Lean

-- recursion, subtraction
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)



-- wildcards and recursion
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs


-- list concatenation
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


-- ordered pairs, TODO
cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct [] _ = []
cartesianProduct _ [] = []
cartesianProduct (x:xs) ys = [(x, y) | y <- ys] ++ cartesianProduct xs ys


-- where keyword
circleArea :: Float -> Float
circleArea radius = piVal * radiusSquared
  where
    piVal = 3.14159          -- Define the value of π
    radiusSquared = radius * radius -- Calculate the radius squared


-- more complicated where clause + guards
isPrime :: Int -> Bool
isPrime n
  | n <= 1    = False
  | otherwise = not (hasDivisor 2)
  where
    hasDivisor d
      | d * d > n      = False
      | n `mod` d == 0 = True
      | otherwise      = hasDivisor (d + 1)

main = do
    print $  fib 10
    print $ listLength [1,2,3,4,5]
    print $ myReverse [1,2,3,4,5]
    print $ cartesianProduct [1,2] [4,5,6]
    print $ isPrime 77