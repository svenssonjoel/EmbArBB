import Intel.ArBB 

import qualified Data.Vector.Storable as V 



matVec :: Exp (DVector Dim2 Float) -> Exp (DVector Dim1 Float) -> Exp (DVector Dim1 Float) 
matVec m v = addReduce0 
             $ m * (repeatRow v (getNRows m))


testMatVec = 
  withArBB $ 
  do 
     f <- capture2 matVec  
     let m1 = Vector (V.fromList [2,0,0,0,
                                  0,2,0,0,
                                  0,0,2,0,
                                  0,0,0,2]) (Two 4 4) 
         v1 = fromList [1,2,3,4] 
     
     r1 <- liftIO$ new1D 4

     execute2 f (m1 :- v1)  r1
              
     r <- liftIO$ freeze r1
              
     liftIO$ putStrLn$ show r

main = testMatVec