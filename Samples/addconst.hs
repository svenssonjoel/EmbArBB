import Intel.ArBB 

import qualified Data.Vector.Storable as V 
import Prelude hiding (length)


addconst :: Num a => Exp a -> Exp (DVector Dim1 a) -> Exp (DVector Dim1 a) 
addconst s v = v + ss 
    where 
      ss = constVector s (length v) 

main = 
  withArBB $ 
  do 
     f <- capture addconst
             
     x <- copyIn $ mkDVector (V.fromList [1..10 :: Float]) (Z:.10)

     r1 <- new (Z:.10) 0 
     
     c <- mkScalar 1 
     
     execute f (c :- x)  r1
              
     r <- copyOut r1
              
     liftIO$ putStrLn$ show r
