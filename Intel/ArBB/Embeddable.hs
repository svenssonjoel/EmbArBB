module Intel.ArBB.Embeddable where 

import Intel.ArBB.Types 
import qualified Intel.ArbbVM as VM

import Data.Word 
import Data.Int 

class Embeddable a where 
  typeOf :: a -> Type 
  

----------------------------------------------------------------------------
-- Base embeddable 

instance Embeddable Int32 where 
  typeOf _ = Scalar VM.ArbbI32 

instance Embeddable Word32 where 
  typeOf _ = Scalar VM.ArbbU32 

instance Embeddable Float where 
  typeOf _ = Scalar VM.ArbbF32 
  