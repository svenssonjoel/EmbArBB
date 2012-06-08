import Intel.ArBB 

import qualified Data.Vector.Storable as V 
import Prelude hiding (length)


axpy :: Num a => Exp a -> Exp (DVector Dim1 a) -> Exp (DVector Dim1 a) -> Exp (DVector Dim1 a) 
axpy s x y = (ss*x) + y 
    where 
      ss = constVector s (length x) 


testSaxpy = 
  withArBB $ 
  do 
     f <- capture axpy
     let x = fromList [1,3..10 :: Float] 
         y = fromList [2,4..10 :: Float] 
        
     
     r1 <- liftIO$ new1D 5   

     execute f (1 :- x :- y)  r1
              
     r <- liftIO$ freeze r1
              
     liftIO$ putStrLn$ show r


testDaxpy = 
  withArBB $ 
  do 
     f <- capture axpy
     let x = Vector (V.fromList [1,2,3,4,5,6,7,8,9,10 :: Double]) (One 10) 
         y = Vector (V.fromList [1,2,3,4,5,6,7,8,9,10 :: Double]) (One 10 ) 
        
     
     r1 <- liftIO$ new1D 10   

     execute f (1 :- x :- y)  r1
              
     r <- liftIO$ freeze r1
              
     liftIO$ putStrLn$ show r


main = testSaxpy