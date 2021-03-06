import Intel.ArBB 

import qualified Data.Vector.Storable as V 



matVec :: Exp (DVector Dim2 Float) -> Exp (DVector Dim1 Float) -> Exp (DVector Dim1 Float) 
matVec m v = addReduce rows 
             $ m * (repeatRow (getNRows m) v)



main =  
  withArBB $ 
  do 
     f <- capture matVec  
     let m1 = V.fromList [2,0,0,0,
                          0,2,0,0,
                          0,0,2,0,
                          0,0,0,2]
         v1 = V.fromList [1,2,3,4] 
     
     m <- copyIn $ mkDVector m1 (Z:.4:.4) 
     v <- copyIn $ mkDVector v1 (Z:.4) 

     r1 <- new (Z:.4) 0 

     execute f (m :- v)  r1
              
     r <- copyOut r1
              
     liftIO$ putStrLn$ show r
