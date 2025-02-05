module Simple where 
    

{- Testing class declarations and instance declarations -}

class YesNo a where 
  yesno :: a -> Bool 

instance YesNo Int where 
  yesno 0 = False 
  yesno _ = True 

instance YesNo [a] where 
  yesno [] = False 
  yesno _ = True 

instance YesNo Bool where 
  yesno = id 

instance YesNo (Maybe a) where 
  yesno (Just _) = True 
  yesno Nothing = False 



main = do 
  -- print $ yesno $ 3
  -- print $ yesno "haha"
  -- print $ yesno ""
  print $ yesno $ Just 0
  print $ yesno True 
  -- print $ yesno [] 
  print $ yesno [1,2,3]



-- addVals :: Int -> Int -> Int 
-- addVals a b = a + b 


-- random :: Int -> [Int] -> Int 
-- random x [] = x 
-- random x (y:ys) = x - 1


-- insert :: Int -> [Int] -> [Int]
-- insert x [] = [x]
-- insert x (y:ys) = if x < y then x:y:ys else y : insert x ys

