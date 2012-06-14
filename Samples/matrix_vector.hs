import Intel.ArBB 

import qualified Data.Vector.Storable as V 



matVec :: Exp (DVector Dim2 Float) -> Exp (DVector Dim1 Float) -> Exp (DVector Dim1 Float) 
matVec m v = addReduce0 
             $ m * (repeatRow v (getNRows m))



main =  
  withArBB $ 
  do 
     f <- capture matVec  
     let m1 = V.fromList [2,0,0,0,
                          0,2,0,0,
                          0,0,2,0,
                          0,0,0,2]
         v1 = V.fromList [1,2,3,4] 
     
     m <- copyIn m1 (4 :. 4 :. Z) 
     v <- copyIn v1 (4 :. Z) 

     r1 <- new (4 :. Z) 0 

     execute f (m :- v)  r1
              
     r <- copyOut r1
              
     liftIO$ putStrLn$ show r
