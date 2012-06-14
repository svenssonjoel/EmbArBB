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
      let mult = a * repeatRow (extractCol b i) m 
          col  = addReduce0 mult 
      in (replaceCol c i col, i+1) 




testMatMul = 
  withArBB $ 
  do 
     f <- capture matmul
    
     m1 <- copyIn (V.fromList [2,0,0,
                               0,2,0,
                               0,0,2])
                  (3:.3:.Z) 
     m2 <- copyIn (V.fromList [1,2,3,
                               4,5,6,
                               7,8,9])
                  (3:.3:.Z) 
            
     r1 <- new (3:.3:.Z) 0
  
     execute f (m1 :- m2)  r1      
                  
     r <- copyOut r1
              
     liftIO$ putStrLn$ show r


main = testMatMul
 
