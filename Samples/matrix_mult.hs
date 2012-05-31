import Intel.ArBB 

import qualified Data.Vector.Storable as V 

import System.Time
import Text.Printf


matmul :: Exp (DVector Dim2 Float) 
        -> Exp (DVector Dim2 Float) 
        -> Exp (DVector Dim2 Float) 
matmul a b = fst $ while cond body (a,0)
  where 
    m = getNRows a
    n = getNCols b 
    cond (c,i) = i <* n
    body (c,i) = 
      let mult = a * repeatRow (extractCol b i) m 
                 col  = addReduce0 mult 
      in (replaceCol c i col, i+1) 

testMatMul = 
  withArBB $ 
  do 
     f <- capture2 matmul
     
     --str <- serialize f
     --liftIO$ putStrLn str

                                                   --  W   H
     let m1 = Vector (V.fromList [1..(320*640)]) (Two 640 320) 
         m2 = Vector (V.fromList [1..(640*320)]) (Two 320 640)  
     r1 <- liftIO$ new2D 320 320
    -- r2 <- liftIO$ new2D 1000 1000   
  
     execute2 f (m1 :- m2)  r1      
     
     t1 <- liftIO getClockTime 
     execute2 f (m1 :- m2)  r1      
     --finish 
     
     t2 <- liftIO getClockTime 


     --liftIO $ putStrLn $ show b
              
     r <- liftIO$ freeze r1
              
     -- liftIO$ putStrLn$ show r

     -- r <- liftIO$ freeze r2
              
     -- liftIO$ putStrLn$ show r

           
     --liftIO $ putStrLn $ show  (diffUTCTime t2 t1)
     liftIO $ printf "%f\n"  (diffs (diffClockTimes t2 t1))
    


picoToMs p = (fromIntegral p) * 1E-9

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

main = testMatMul

