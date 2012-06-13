import Intel.ArBB 

import qualified Data.Vector.Storable as V 
import Prelude hiding (length)

import Control.Monad.IO.Class

addconst :: Num a => Exp a -> Exp (DVector Dim1 a) -> Exp (DVector Dim1 a) 
addconst s v = v + ss 
    where 
      ss = constVector s (length v) 

main = 
  withArBB $ 
  do 
     f <- capture addconst
             
     x <- copyIn (V.fromList [1..10 :: Float]) ((10:: Int) :. Z)

     r1 <- new ((10 :: Int) :. Z) 0 

     execute f (1 :- x)  r1
              
     r <- copyOut r1
              
     liftIO$ putStrLn$ show r
