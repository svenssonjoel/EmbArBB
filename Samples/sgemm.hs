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
     f <- capture sgemm
     str <- serialize f
     liftIO$ putStrLn str


     m1 <- copyIn (V.fromList [1,2,3,1,2,3,1,2,3]) (Z:.3:.3) 
     m2 <- copyIn (V.fromList [2,0,0,0,2,0,0,0,2]) (Z:.3:.3) 
     m3 <- copyIn (V.fromList [1,0,0,0,1,0,0,0,1]) (Z:.3:.3) 

     r1 <- new (Z:.3:.3) 0   

     execute f (m1 :- m2 :- m3 :- 1 :- 1)  r1
              
     r <- copyOut r1
              
     liftIO$ putStrLn$ show r


main = testSgemm

