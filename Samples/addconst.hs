import Intel.ArBB 

import qualified Data.Vector.Storable as V 
import Prelude hiding (length)


addconst :: Num a => Exp a -> Exp (DVector Dim1 a) -> Exp (DVector Dim1 a) 
addconst s v = v + ss 
    where 
      ss = constVector s (length v) 


testAddconst = 
  withArBB $ 
  do 
     f <- capture addconst
     let x = Vector (V.fromList [1,2,3,4,5,6,7,8,9,10 :: Float]) (One 10)   
     
     r1 <- liftIO$ new1D 10   

     execute f (1 :- x)  r1
              
     r <- liftIO$ freeze r1
              
     liftIO$ putStrLn$ show r



main = 
  withArBB $ 
  do 
     f <- capture addconst
    
     let x = fromList [0..10 :: Float] -- Vector vData (One 10)   
     
     r1 <- liftIO$ new1D 10   

     execute f (1 :- x)  r1
              
     r <- liftIO$ freeze r1
              
     liftIO$ putStrLn$ show r
--  where 
--   vData = V.fromList [0..10 :: Float]            