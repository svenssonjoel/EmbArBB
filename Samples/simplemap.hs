import Intel.ArBB 

import qualified Data.Vector.Storable as V 
import Prelude hiding (length,map)
import Data.Word


addOne :: Exp Word32 -> Exp Word32
addOne x = x + 1

-- I need to pass the function to be mapped. Not a very nice interface. 
-- TODO: Find a way to do this that makes more sense. 
mapper :: Function (EIn (Exp Word32) (Exp Word32)) (EOut (Exp Word32)) -> Exp (Vector Word32) -> Exp (Vector Word32) 
mapper f v = map f v 

getCoord :: Exp Word32 -> Exp Word32
getCoord x = x + getNeighbor x 0 0 (-1)


mapper2 :: Function (EIn (Exp Word32) (Exp Word32)) (EOut (Exp Word32)) -> Exp (Vector Word32) -> Exp (Vector Word32) 
mapper2 f v = map f v 


testMap = 
  withArBB $ 
  do 
     f <- capture2 addOne      

     strf <- serialize f
     liftIO$ putStrLn strf

     m <- capture2 (mapper f) 

     strm <- serialize m 
     liftIO$ putStrLn strm

--------------------------------
     f2 <- capture2 getCoord     

     strf <- serialize f2
     liftIO$ putStrLn strf

     m2 <- capture2 (mapper2 f2) 

     strm <- serialize m2 
     liftIO$ putStrLn strm

             

     let x = Vector (V.fromList [1,2,3,4,5,6,7,8,9,10 :: Word32]) (One 10) 
       
        
     r1 <- liftIO$ new1D 10   
     r2 <- liftIO$ new1D 10   

     execute2 m  (x)  r1
     execute2 m2 (x)  r2
              
     r <- liftIO$ freeze r1
     r' <- liftIO$ freeze r2
              
     liftIO$ putStrLn$ show r
     liftIO$ putStrLn$ show r'


main = testMap