{-# LANGUAGE ScopedTypeVariables #-}
import Intel.ArBB 

import qualified Data.Vector.Storable as V 
-- import qualified Data.Vector.Random.Mersenne as G
import System.Random.Mersenne
import System.Time
import Text.Printf
import System.Environment

import GHC.Float

import Prelude as P

matmul :: Exp (DVector Dim2 Float) 
        -> Exp (DVector Dim2 Float) 
        -> Exp (DVector Dim2 Float) 
matmul a b = fst $ while cond body (a,0)
  where 
    m = getNRows a
    n = getNCols b 
    cond (c,i) = i <* n
    body (c,i) = 
      let mult = a * repeatRow m (extractCol i b)
          col  = addReduce rows mult 
      in (replaceCol i col c, i+1) 




testMatMul g size  = 
  withArBB $ 
  do 
     tc1 <- liftIO getClockTime 
     f <- capture matmul
     tc2 <- liftIO getClockTime 
     
     --str <- serialize f
     --liftIO$ putStrLn str
     (rs1 :: [Double]) <- liftIO$ randoms g
     (rs2 :: [Double]) <- liftIO$ randoms g
                                                             
     m1 <- copyIn $ mkDVector (V.fromList (take (size*size) (P.map double2Float rs1))) (Z:.size:.size) 
     m2 <- copyIn $ mkDVector (V.fromList (take (size*size) (P.map double2Float rs2))) (Z:.size:.size) 
     r1 <- new  (Z:.size:.size) 0
  
     -- warmup
     execute f (m1 :- m2)  r1      
     finish      

     t1 <- liftIO getClockTime 
     execute f (m1 :- m2)  r1      
     finish 
     
     t2 <- liftIO getClockTime 


     --liftIO $ putStrLn $ show b
              
     r <- copyOut r1
              
     -- liftIO$ putStrLn$ show r

     -- r <- liftIO$ freeze r2
              
     -- liftIO$ putStrLn$ show r

           
     --liftIO $ putStrLn $ show  (diffUTCTime t2 t1)
     liftIO $ printf "%f\t%f\n"  (diffms (diffClockTimes t2 t1))
                                 (diffms (diffClockTimes tc2 tc1))

    


picoToMs p = (fromIntegral p) * 1E-9

diffms :: TimeDiff -> Float
diffms diff | tdYear diff == 0 && 
              tdMonth diff == 0 && 
              tdDay diff == 0 && 
              tdMin diff == 0 && 
              tdHour diff == 0  =  (fromIntegral ps) * 1E-9  + 
                                   (fromIntegral sec) * 1000                
             where 
               ps  = tdPicosec diff 
               sec = tdSec diff


main =
 do 
   args <- getArgs 
   case P.length args of 
     1 -> 
         do g <- newMTGen Nothing
            testMatMul g (read (head args) :: Int)
     n -> error "incorrect arg" 
 
