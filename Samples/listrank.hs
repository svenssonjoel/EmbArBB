{-# LANGUAGE TypeOperators #-} 
import Intel.ArBB

import Intel.ArBB.Data.Boolean
import Intel.ArBB.Literal

import qualified Data.Vector.Storable as V 
import Prelude hiding (length,zipWith)

import Data.Int


mark :: Exp Int32 -> Exp Int32 -> Exp Int32
mark a b = ifThenElse (a ==* b) 0 1 

listRank :: Exp (DVector Dim1 Int32) -> Exp (DVector Dim1 Int32) 
listRank pt =  val -- pointerJump pt val
    where
      val = zipWith mark ixs pt
      ixs = indices 0 8 1


pointerJump :: Exp (DVector Dim1 Int32)
               -> Exp (DVector Dim1 Int32)
               -> Exp (DVector Dim1 Int32)
pointerJump pt val =
   (\(_,x) -> x) $ while cond body (pt,val)
  where
    cond (npt,val) = isNotEqual npt val
    body (npt,val) = (npt, --gather1D (vecToUSize npt) 0 npt,
                             val) --(gather1D (vecToUSize npt) 0 val))


isNotEqual :: Exp (DVector Dim1 Int32)
              -> Exp (DVector Dim1 Int32)
              -> Exp Boolean
isNotEqual v1 v2 = index0$ andReduce rows (zipWith (==*) v1 v2)  
             



main = 
  withArBB $ 
  do 
     --f <- capture listRank
     --str <- serialize f
     --liftIO$ putStrLn str

     --p <- capture isNotEqual -- pointerJump
     --str <- serialize p
     --liftIO$ putStrLn str

     q <- capture  pointerJump
     str <- serialize q
     liftIO$ putStrLn str

     
     --x <- copyIn $ mkDVector
     --              (V.fromList [2,5,1,3,7,6,6,3 :: Int32]) (Z:.8)
     --r1 <- new (Z:.8) 0 
     --execute f x  r1              
     --r <- copyOut r1              
     --liftIO$ putStrLn$ show r
