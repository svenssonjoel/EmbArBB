import Intel.ArBB 

import qualified Data.Vector.Storable as V 



sgemm :: Exp (DVector Dim2 Float) 
        -> Exp (DVector Dim2 Float) 
        -> Exp (DVector Dim2 Float) 
        -> Exp Float
        -> Exp Float
        -> Exp (DVector Dim2 Float) 
sgemm a b c alpha beta = fst $ while cond body (c,0)
  where 
    valpha = constVector alpha m 
    vbeta  = constVector beta n 
    m = getNRows a
    n = getNCols b 
    cond (c,i) = i <* n
    body (c,i) = let mult = a * repeatRow (extractCol b i) m 
                     col  = valpha * addReduce0 mult + vbeta * (extractCol c i)
                 in (replaceCol c i col, i+1) 

testSgemm = 
  withArBB $ 
  do 
     f <- capture2 sgemm
     str <- serialize f
     liftIO$ putStrLn str


     let m1 = Vector (V.fromList [1,2,3,1,2,3,1,2,3]) (Two 3 3) 
         m2 = Vector (V.fromList [2,0,0,0,2,0,0,0,2]) (Two 3 3)  
         m3 = Vector (V.fromList [1,0,0,0,1,0,0,0,1]) (Two 3 3)  
     r1 <- liftIO$ new2D 3 3   

     execute2 f (m1 :- m2 :- m3 :- 1 :- 1)  r1
              
     r <- liftIO$ freeze r1
              
     liftIO$ putStrLn$ show r
