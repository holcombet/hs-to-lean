module Test (recursiveAdd) where

recursiveAdd :: Int -> Int -> Int
recursiveAdd n 0 = n
recursiveAdd n m = 1 + recursiveAdd n (m-1)


newtype UserId = UserId Int deriving (Show)

myId :: UserId
myId =  UserId 10

main = do 
    print myId

