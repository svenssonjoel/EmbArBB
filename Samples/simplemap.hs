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


-- TODO: This causes: Dynamic compiler error.. 
testMap = 
  withArBB $ 
  do 
     f <- capture2 addOne 

     strf <- serialize f
     liftIO$ putStrLn strf

     m <- capture2 (mapper f) 

     strm <- serialize m 
     liftIO$ putStrLn strm
             

     let x = Vector (V.fromList [1,2,3,4,5,6,7,8,9,10 :: Word32]) (One 10) 
       
        
     r1 <- liftIO$ new1D 10   

     execute2 m (x)  r1
              
     r <- liftIO$ freeze r1
              
     liftIO$ putStrLn$ show r


main = testMap