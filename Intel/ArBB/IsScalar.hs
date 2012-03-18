
module Intel.ArBB.IsScalar where 

import Intel.ArBB.Types 
import qualified Intel.ArbbVM as VM

import Data.Int
import Data.Word

----------------------------------------------------------------------------
-- The supported basetypes 

class IsScalar a where 
  scalarType :: a -> Type 
  
instance IsScalar Int8 where 
  scalarType _ = Scalar VM.ArbbI8 
  
instance IsScalar Int32 where 
  scalarType _ = Scalar VM.ArbbI32
