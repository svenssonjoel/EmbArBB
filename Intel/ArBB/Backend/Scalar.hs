
module Intel.ArBB.Backend.Scalar where 


data BEScalar a = BEScalar {beScalarID :: Integer} 

-- for now, this is taken care of by specialized code in Backend.ArBB
--class Scalar a where 
--  mkScalar :: BackendMonad m => a -> m (BEScalar a) 
  
  

