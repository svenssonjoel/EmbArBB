{-# LANGUAGE ScopedTypeVariables #-}
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

class EmbeddableExp a where 
  typeOfExp :: Exp a -> Type 
  
instance EmbeddableExp Int32 where 
  typeOfExp _ = Scalar VM.ArbbI32 

instance EmbeddableExp Word32 where 
  typeOfExp _ = Scalar VM.ArbbU32 

instance EmbeddableExp Float where 
  typeOfExp _ = Scalar VM.ArbbF32 
  
instance EmbeddableExp Double where 
  typeOfExp _ = Scalar VM.ArbbF64 
  
instance EmbeddableExp USize where 
  typeOfExp _ = Scalar VM.ArbbUsize

instance EmbeddableExp ISize where 
  typeOfExp _ = Scalar VM.ArbbIsize


instance (Embeddable a, Embeddable b) => EmbeddableExp (a,b)  where 
  typeOfExp e = Tuple [typeOf (undefined ::a),typeOf (undefined :: b)]


-- TODO: How to really do the above. 
--   It feels very flaky.. 
--   But using the EmbeddableExp class makes the capture function 
--   work on (a -> Exp (b,c)) functions  as well.
--   With same result as a (a -> (Exp b, Exp c)) one. 