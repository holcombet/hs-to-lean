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
  print $ yesno $ Just 0
  print $ yesno True 
  print $ yesno [1,2,3]

