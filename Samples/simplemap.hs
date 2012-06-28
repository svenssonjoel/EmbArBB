import Intel.ArBB 

import qualified Data.Vector.Storable as V 
import Prelude hiding (length,map)
import Data.Word


addOne :: Exp Word32 -> Exp Word32
addOne x = x + 1

mapper :: Exp (Vector Word32) -> Exp (Vector Word32) 
mapper v = map addOne v 

getCoord :: Exp Word32 -> Exp Word32
getCoord x = x + getNeighbor x 0 0 (-1)

mapper2 ::  Exp (Vector Word32) -> Exp (Vector Word32) 
mapper2 v = map getCoord v 


testMap = 
  withArBB $ 
  do 
     f <- capture addOne      

     strf <- serialize f
     liftIO$ putStrLn strf

     m <- capture mapper 

     strm <- serialize m 
     liftIO$ putStrLn strm

--------------------------------
     f2 <- capture getCoord     

     strf <- serialize f2
     liftIO$ putStrLn strf

     m2 <- capture mapper2 

     strm <- serialize m2 
     liftIO$ putStrLn strm

            
       
     x <- copyIn $ mkDVector (V.fromList [1..10::Word32]) (Z:.10) 
        
     r1 <- new (Z:.10) 0
     r2 <- new (Z:.10) 0

     execute m  x  r1
     execute m2 x  r2
              
     r  <- copyOut r1
     r' <- copyOut r2
              
     liftIO$ putStrLn$ show r
     liftIO$ putStrLn$ show r'


main = testMap