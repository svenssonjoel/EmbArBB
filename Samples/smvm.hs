
import Intel.ArBB

smvm :: Exp (DVector Dim1 Float) 
     -> Exp (DVector Dim1 USize) 
     -> Exp (DVector Dim1 USize) 
     -> Exp (DVector Dim1 Float)
     -> Exp (DVector Dim1 Float) 
smvm mval cidx os vec = addReduceSeg nps 
    where 
      ps = mval * gather1D cidx 0 vec
      nps = applyNesting offsets os ps 
  
main = putStrLn "Implement a real main" 