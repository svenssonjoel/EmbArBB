{-# LANGUAGE ScopedTypeVariables #-}
import Intel.ArBB 

import qualified Data.Vector.Storable as V 


matmul :: Exp (DVector Dim2 Float) 
        -> Exp (DVector Dim2 Float) 
        -> Exp (DVector Dim2 Float) 
matmul a b = fst $ while cond body (a,0)
  where 
    m = getNRows a
    n = getNCols b 
    cond (c,i) = i <* n
    body (c,i) = 
      let mult = a * repeatRow m (extractCol i b)
          col  = addReduce rows mult 
      in (replaceCol i col c, i+1) 




testMatMul = 
  withArBB $ 
  do 
     f <- capture matmul
    
     m1 <- copyIn $ mkDVector (V.fromList [2,0,0,
                                           0,2,0,
                                           0,0,2])
                              (Z:.3:.3) 
     m2 <- copyIn $ mkDVector (V.fromList [1,2,3,
                                           4,5,6,
                                           7,8,9])
                              (Z:.3:.3) 
            
     r1 <- new (Z:.3:.3) 0
  
     execute f (m1 :- m2)  r1      
                  
     r <- copyOut r1
              
     liftIO$ putStrLn$ show r


main = testMatMul
 
