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
    valpha = constVector m alpha 
    vbeta  = constVector n beta 
    m = getNRows a
    n = getNCols b 
    cond (c,i) = i <* n
    body (c,i) = let mult = a * repeatRow m (extractCol i b) 
                     col  = valpha * addReduce rows mult + vbeta * (extractCol i c)
                 in (replaceCol i col c, i+1) 

testSgemm = 
  withArBB $ 
  do 
     f <- capture sgemm
     str <- serialize f
     liftIO$ putStrLn str


     m1 <- copyIn $ mkDVector (V.fromList [1,2,3,1,2,3,1,2,3]) (Z:.3:.3) 
     m2 <- copyIn $ mkDVector (V.fromList [2,0,0,0,2,0,0,0,2]) (Z:.3:.3) 
     m3 <- copyIn $ mkDVector (V.fromList [1,0,0,0,1,0,0,0,1]) (Z:.3:.3) 

     r1 <- new (Z:.3:.3) 0   
     
     c <- mkScalar 1

     execute f (m1 :- m2 :- m3 :- c :- c)  r1
              
     r <- copyOut r1
              
     liftIO$ putStrLn$ show r


main = testSgemm

