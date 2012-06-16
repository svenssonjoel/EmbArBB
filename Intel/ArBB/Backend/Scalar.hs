
module Intel.ArBB.Backend.Scalar where 


data BEScalar a = BEScalar {beScalarID :: Integer} 

class Scalar a where 
  mkScalar :: Monad m => a -> m (BEScalar a) 
  
  