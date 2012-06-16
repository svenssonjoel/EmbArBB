{-# LANGUAGE ScopedTypeVariables #-}
import Intel.ArBB 

import qualified Data.Vector.Storable as V 
import Prelude hiding (length)

import Data.IORef

import System.Random.Mersenne
import System.Time
import Text.Printf
import System.Environment

import Prelude as P


dotprod :: Exp (DVector Dim1 Double) -> Exp (DVector Dim1 Double) -> Exp Double 
dotprod v1 v2 = index0$ addReduce0 (v1 * v2)


testDot g size = 
  withArBB $ 
  do 
     f <- capture dotprod

     (rs1 :: [Double]) <- liftIO$ randoms g
     (rs2 :: [Double]) <- liftIO$ randoms g

     x <- copyIn (V.fromList (take size (rs1))) (Z:.size) 
     y <- copyIn (V.fromList (take size (rs2))) (Z:.size) 
     --let a = fromList [0,1,2 :: Double] -- (One 3)   
     --    b = fromList [0,1,2 :: Double] -- (One 3)     

     r1 <- mkRef 0 
   
     -- execute2 f (a :- b)  r2

     t1 <- liftIO getClockTime 
     execute f (x :- y)  r1
     t2 <- liftIO getClockTime          

     --r <- liftIO$ readIORef r1
   
              
     --liftIO$ putStrLn$ show r ++ " " ++ show r'
     liftIO $ printf "%f\n"  (diffs (diffClockTimes t2 t1))
            

diffs :: TimeDiff -> Float
diffs diff | tdYear diff == 0 && 
              tdMonth diff == 0 && 
              tdDay diff == 0 && 
              tdHour diff == 0  =  (fromIntegral ps) * 1E-12  + 
                                   (fromIntegral sec) + 
                                   (fromIntegral min) * 60
                                                    
             where 
               ps  = tdPicosec diff 
               sec = tdSec diff
               min = tdMin diff

             
main = 
    do 
      args <- getArgs 
      case P.length args of 
        1 -> 
            do g <- newMTGen Nothing
               testDot g (read (head args) :: Int)
        n -> error "incorrect arg" 
