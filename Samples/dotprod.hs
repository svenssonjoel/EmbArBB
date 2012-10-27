import Intel.ArBB 

import qualified Data.Vector.Storable as V 
import Prelude hiding (length)

import Data.IORef

dotprod :: Exp (DVector Dim1 Float) -> Exp (DVector Dim1 Float) -> Exp Float 
dotprod v1 v2 = index0$ addReduce rows (v1 * v2)


testDot = 
  withArBB $ 
  do 
     f <- capture dotprod
     let x = V.fromList [1,3..100 :: Float] 
         y = V.fromList [2,4..100 :: Float] 

     x' <- copyIn $ mkDVector x (Z:.100)
     y' <- copyIn $ mkDVector y (Z:.100)
     
     r1 <- mkScalar 0   

     execute f (x' :- y')  r1
              
     r <- readScalar r1
              
     liftIO$ putStrLn$ show r

main = testDot