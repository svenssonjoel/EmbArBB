{-# LANGUAGE FlexibleInstances, 
             ScopedTypeVariables#-}
module Intel.ArBB.Embeddable where 

import Intel.ArBB.Types 
import Intel.ArBB.Syntax
import qualified Intel.ArbbVM as VM


import Data.Word 
import Data.Int 

import Intel.ArBB.Data.Int

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
  
instance Embeddable Double where 
  typeOf _ = Scalar VM.ArbbF64 
  
instance Embeddable USize where 
  typeOf _ = Scalar VM.ArbbUsize

instance Embeddable ISize where 
  typeOf _ = Scalar VM.ArbbIsize

instance (Embeddable a, Embeddable b) => Embeddable (a,b)  where 
  typeOf (a,b) = Tuple [typeOf a,typeOf b]
  
  
----------------------------------------------------------------------------
-- Experiment 

--class EmbeddableExp a where 
--  typeOfExp :: Exp a -> Type 
  
instance Embeddable (Exp Int32) where 
  typeOf _ = Scalar VM.ArbbI32 

instance Embeddable (Exp Word32) where 
  typeOf _ = Scalar VM.ArbbU32 

instance Embeddable (Exp Float) where 
  typeOf _ = Scalar VM.ArbbF32 
  
instance Embeddable (Exp Double) where 
  typeOf _ = Scalar VM.ArbbF64 
  
instance Embeddable (Exp USize) where 
  typeOf _ = Scalar VM.ArbbUsize

instance Embeddable (Exp ISize) where 
  typeOf _ = Scalar VM.ArbbIsize


instance (Embeddable a, Embeddable b) => Embeddable (Exp (a,b))  where 
  typeOf e = Tuple [typeOf (undefined ::a),typeOf (undefined :: b)]


-- TODO: How to really do the above. 
--   It feels very flaky.. 
--   But using the EmbeddableExp class makes the capture function 
--   work on (a -> Exp (b,c)) functions  as well.
--   With same result as a (a -> (Exp b, Exp c)) one. 