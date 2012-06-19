{- 2012 Joel Svensson -} 

module Intel.ArBB.Types where 

import qualified Intel.ArbbVM as VM
import qualified Intel.ArbbVM.Convenience as VM 

---------------------------------------------------------------------------- 
-- What types can we have ? 
data Type = Scalar VM.ScalarType 
          | Dense Dimensionality VM.ScalarType 
          | Nested VM.ScalarType
                        
          -- Tuple types 
          | Tuple [Type] 
-- EXPERIMENTAL function types
          | Type :-> Type
          deriving (Eq,Show)                  
infixr :-> 

data Dimensionality = I | II | III 
                    deriving (Eq, Show)

---------------------------------------------------------------------------- 
--  Type  helpers
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


----------------------------------------------------------------------------
-- get input/output types from a function type 
getInputTypes :: Type -> [Type] 
getInputTypes t@(a :-> b) = getInputTypes' t 
    where 
      getInputTypes' (a :-> b) = a : getInputTypes' b
      getInputTypes' _ = []
getInputTypes _ = error "getInputTypes: not a function type" 

getOutputType :: Type -> Type 
getOutputType (a :-> b) = getOutputType' b
    where 
      getOutputType' (a :-> b) = getOutputType' b
      getOutputType' b         = b 
getOutputType _         = error "getOutputTypes: not a function type" 