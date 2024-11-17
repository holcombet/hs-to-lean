module Calendar where

abc :: Int
abc = 13


bar = 10

(x, y) = (1, 2)

-- test data decl with AppTy
data Shape = Circle Float Float Float | Rectangle Float Float Float Float 

-- test recursive guards
nthElement :: [a] -> Int -> Maybe a 
nthElement [] a = Nothing
nthElement (x:xs) a | a <= 0 = Nothing
                    | a == 1 = Just x
                    | a > 1 = nthElement xs (a-1)

-- Test let keyword
calculateArea :: Float -> Float
calculateArea r = 
    let pi = 3.14
    in pi * r * r

-- Test HsParTy
applyFunc :: (Int -> Int) -> Int -> Int
applyFunc f x = f x


insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) = if x < y 
                 then x:y:ys 
         else y : insert x ys

insertionSort :: [Int] -> [Int]
insertionSort [x] = [x]
insertionSort (x:xs) = insert x (insertionSort xs)

isPrime :: Int -> Bool
isPrime n
  | n <= 1    = False
  | otherwise = not (hasDivisor 2)
  where
    hasDivisor d
      | d * d > n      = False
      | n `mod` d == 0 = True
      | otherwise      = hasDivisor (d + 1)

-- test data decl with deriving 
data DayOfWeek
    = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Eq, Enum, Bounded)

data Month
    = January | February | March     | April   | May      | June
    | July    | August   | September | October | November | December
    deriving (Enum, Bounded, Show)


-- test functions with typeclasses (?)
next :: (Eq a, Enum a, Bounded a) => a -> a
next x | x == maxBound = minBound
       | otherwise     = succ x


-- test case keyword
pad :: Int -> String
pad day = case show day of
    [c] -> [' ', c]
    cs  -> cs

-- test where keyword
month :: Month -> DayOfWeek -> Int -> String
month m startDay maxDay = show m ++ " 2015\n" ++ week ++ spaces Sunday
  where
    week = "Su Mo Tu We Th Fr Sa\n"

    spaces currDay | startDay == currDay = days startDay 1
                   | otherwise           = "   " ++ spaces (next currDay)

    days Sunday    n | n > maxDay = "\n"
    days _         n | n > maxDay = "\n\n"
    days Saturday  n              = pad n ++ "\n" ++ days  Sunday    (succ n)
    days day       n              = pad n ++ " "  ++ days (next day) (succ n)

year = month January   Thursday  31
    ++ month February  Sunday    28
    ++ month March     Sunday    31
    ++ month April     Wednesday 30
    ++ month May       Friday    31
    ++ month June      Monday    30
    ++ month July      Wednesday 31
    ++ month August    Saturday  31
    ++ month September Tuesday   30
    ++ month October   Thursday  31
    ++ month November  Sunday    30
    ++ month December  Tuesday   31


main = putStr year