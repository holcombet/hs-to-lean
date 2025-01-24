
module TestFunctions where




frac :: Rational 
frac = 1 / 2

-- Examples of SynDecls
type Name = String      -- no QualTy, Type only

type ResultFunction a b = a -> Either String b      -- QualTy and function

-- data declarations
data Something a b = Blah a | Bleh b        -- data : QualTy and alternating application to constructors

data Color = Red | Green | Blue         -- data : no QualTy

data Tree a = Empty | Node a (Tree a) (Tree a)      -- data : QualTy and multiple constructor types


----------------------------
-- functions
----------------------------

-- simplest function
add :: Int -> Int -> Int 
add a b = a + b

-- implicit type signature
minus a b = a - b


-- return type only
abc :: Int
abc = 13

{-
random function contains:
    - pattern matching
    - Two parameters
    - List data type used
    - operator
-}
random :: Int -> [Int] -> Int 
random x [] = x 
random x (y : ys) = x - 1


{-
categorizeNumber function contains:
    - guards (simple)
-}
categorizeNumber :: Int -> String
categorizeNumber x
  | x < 0     = "Negative"
  | x == 0    = "Zero"
  | x < 10    = "Small"
  | otherwise = "Large"

-- Test let keyword
calculateArea :: Float -> Float
calculateArea r = 
    let pi = 3.14
    in pi * r * r

calculateRandom :: Int -> Int 
calculateRandom x =
    let y = 10
        z = 2
    in x + y + z

-- test recursive guards
nthElement :: [a] -> Int -> Maybe a 
nthElement [] a = Nothing
nthElement (x:xs) a | a <= 0 = Nothing
                    | a == 1 = Just x
                    | a > 1 = nthElement xs (a-1)

--------------------------------------
-- Currently un-used test functions
--------------------------------------


-- -- test recursive guards
-- nthElement :: [a] -> Int -> Maybe a 
-- nthElement [] a = Nothing
-- nthElement (x:xs) a | a <= 0 = Nothing
--                     | a == 1 = Just x
--                     | a > 1 = nthElement xs (a-1)
                    

-- newtype UserId = UserId Int         -- newtype no QualTy

-- myId :: UserId
-- myId = UserId 10



-- exampleTree :: Tree Int
-- exampleTree = Node 5 (Node 3 Empty Empty) (Node 8 Empty Empty)



-- -- Examples of DataDecls

-- -- data Person = Person { name :: String, age :: Int }  -- data : records









-- -- test case keyword
-- pad :: Int -> String
-- pad day = case show day of
--     [c] -> [' ', c]
--     cs  -> cs

-- greaterThanZero :: (Ord a, Num a) => a -> Bool
-- greaterThanZero x = x > 0

-- data MyType = Foo Int String | Bar 
--     deriving (Eq)

-- example :: MyType -> String
-- example (Foo n s) = s ++ show n
-- example Bar = "No data"


-- applyFunc :: (Int -> Int) -> Int -> Int
-- applyFunc f x = f x




-- -- newtype MyType = MyCon (Either String Bool)






-- -- Test HsParTy




-- bar = 10

-- (x, y) = (1, 2)

-- -- test data decl with AppTy
-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float 



-- -- Test let keyword
-- calculateArea :: Float -> Float
-- calculateArea r = 
--     let pi = 3.14
--     in pi * r * r




-- insert :: Int -> [Int] -> [Int]
-- insert x [] = [x]
-- insert x (y:ys) = if x < y 
--                  then x:y:ys 
--          else y : insert x ys

-- insertionSort :: [Int] -> [Int]
-- insertionSort [x] = [x]
-- insertionSort (x:xs) = insert x (insertionSort xs)

-- isPrime :: Int -> Bool
-- isPrime n
--   | n <= 1    = False
--   | otherwise = not (hasDivisor 2)
--   where
--     hasDivisor d
--       | d * d > n      = False
--       | n `mod` d == 0 = True
--       | otherwise      = hasDivisor (d + 1)

-- -- test data decl with deriving 
-- data DayOfWeek
--     = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
--     deriving (Eq, Enum, Bounded)

-- data Month
--     = January | February | March     | April   | May      | June
--     | July    | August   | September | October | November | December
--     deriving (Enum, Bounded, Show)


-- -- test functions with typeclasses (?)
-- next :: (Eq a, Enum a, Bounded a) => a -> a
-- next x | x == maxBound = minBound
--        | otherwise     = succ x




-- -- test where keyword
-- month :: Month -> DayOfWeek -> Int -> String
-- month m startDay maxDay = show m ++ " 2015\n" ++ week ++ spaces Sunday
--   where
--     week = "Su Mo Tu We Th Fr Sa\n"

--     spaces currDay | startDay == currDay = days startDay 1
--                    | otherwise           = "   " ++ spaces (next currDay)

--     days Sunday    n | n > maxDay = "\n"
--     days _         n | n > maxDay = "\n\n"
--     days Saturday  n              = pad n ++ "\n" ++ days  Sunday    (succ n)
--     days day       n              = pad n ++ " "  ++ days (next day) (succ n)

-- year = month January   Thursday  31
--     ++ month February  Sunday    28
--     ++ month March     Sunday    31
--     ++ month April     Wednesday 30
--     ++ month May       Friday    31
--     ++ month June      Monday    30
--     ++ month July      Wednesday 31
--     ++ month August    Saturday  31
--     ++ month September Tuesday   30
--     ++ month October   Thursday  31
--     ++ month November  Sunday    30
--     ++ month December  Tuesday   31


-- data Tree = Nil | Node Int Tree Tree

-- insert :: Int -> Tree -> Tree
-- insert x Nil = Node x Nil Nil
-- insert x (Node a left right) = 
--     if x < a then Node a (insert x left) right else Node a left (insert x right)

-- heapify :: [Int] -> Tree
-- heapify [] = Nil
-- heapify (x:xs) = insert x (heapify xs)

-- tree2list :: Tree -> [Int]
-- tree2list Nil = []
-- tree2list (Node a left right) = tree2list left ++ [a] ++ tree2list right

-- heapSort = tree2list . heapify 

main = putStr "Hello World"


-- -- Pattern Matching Examples to Translate to Lean

-- -- recursion, subtraction
-- fib :: Int -> Int
-- fib 0 = 0
-- fib 1 = 1
-- fib n = fib (n - 1) + fib (n - 2)



-- -- wildcards and recursion
-- listLength :: [a] -> Int
-- listLength [] = 0
-- listLength (_:xs) = 1 + listLength xs


-- -- list concatenation
-- myReverse :: [a] -> [a]
-- myReverse [] = []
-- myReverse (x:xs) = myReverse xs ++ [x]


-- -- ordered pairs, TODO
-- cartesianProduct :: [a] -> [b] -> [(a, b)]
-- cartesianProduct [] _ = []
-- cartesianProduct _ [] = []
-- cartesianProduct (x:xs) ys = [(x, y) | y <- ys] ++ cartesianProduct xs ys


-- -- where keyword
-- circleArea :: Float -> Float
-- circleArea radius = piVal * radiusSquared
--   where
--     piVal = 3.14159          -- Define the value of π
--     radiusSquared = radius * radius -- Calculate the radius squared


-- -- more complicated where clause + guards
-- isPrime :: Int -> Bool
-- isPrime n
--   | n <= 1    = False
--   | otherwise = not (hasDivisor 2)
--   where
--     hasDivisor d
--       | d * d > n      = False
--       | n `mod` d == 0 = True
--       | otherwise      = hasDivisor (d + 1)

-- main = do
--     print $  fib 10
--     print $ listLength [1,2,3,4,5]
--     print $ myReverse [1,2,3,4,5]
--     print $ cartesianProduct [1,2] [4,5,6]
--     print $ isPrime 77