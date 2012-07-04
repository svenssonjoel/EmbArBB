import Intel.ArBB 

import qualified Data.Vector.Storable as V 
import Prelude hiding (length)


saxpy :: Num a => Exp a -> Exp (DVector Dim1 a) -> Exp (DVector Dim1 a) -> Exp (DVector Dim1 a) 
saxpy s x y = (ss*x) + y 
    where 
      ss = constVector (length x) s 





main = 
  withArBB $ 
  do 
     f <- capture saxpy
     
     let v1 = V.fromList [1,3..10::Float] 
         v2 = V.fromList [2,4..10::Float] 
          
     x <- copyIn $ mkDVector v1 (Z:.5) 
     y <- copyIn $ mkDVector v2 (Z:.5) 
     
     r1 <- new (Z:.5) 0    
           
     c <- mkScalar 1 
     
     execute f (c :- x :- y)  r1
              
     r <- copyOut r1
              
     liftIO$ putStrLn$ show r