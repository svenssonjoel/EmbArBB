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
   
     x <- copyIn (V.fromList [1,3..10::Float]) (Z:.5) 
     y <- copyIn (V.fromList [2,4..10::Float]) (Z:.5) 
     
     r1 <- new (Z:.5) 0    

     execute f (1 :- x :- y)  r1
              
     r <- copyOut r1
              
     liftIO$ putStrLn$ show r

main = testSaxpy