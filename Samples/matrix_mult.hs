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
     
     
                                            --  W H
     let m1 = Vector  (V.fromList [2,0,0,
                                   0,2,0,
                                   0,0,2]) (Two 3 3) 
         m2 = Vector  (V.fromList [1,2,3,
                                   4,5,6,
                                   7,8,9]) (Two 3 6)  
     r1 <- liftIO$ new2D 3 3
  
     execute f (m1 :- m2)  r1      
                  
     r <- liftIO$ freeze r1
              
     liftIO$ putStrLn$ show r

main = testMatMul
 
