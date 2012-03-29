{- 2012 Joel Svensson -} 

module Intel.ArBB.Types where 

import qualified Intel.ArbbVM as VM
import qualified Intel.ArbbVM.Convenience as VM 

---------------------------------------------------------------------------- 
-- What types can we have ? 
data Type = Scalar VM.ScalarType 
          | Dense Dimensionality VM.ScalarType 
                        
          -- Tuple types 
          | Tuple [Type] 
          deriving (Eq,Show)                  

data Dimensionality = I | II | III 
                    deriving (Eq, Show)


isScalar :: Type -> Bool 
isScalar (Scalar _) = True
isScalar _ = False 

is1D :: Type -> Bool 
is1D (Dense I _) = True 
is1D _ = False 

is2D (Dense II _) = True 
is2D _ = False 

is3D (Dense III _) = True
is3D _ = False

decrRank (Dense III a) = Just$ Dense II a 
decrRank (Dense II a) = Just$ Dense I a 
decrRank (Dense I a) = Just$ Scalar a 
decrRank a = Nothing 
