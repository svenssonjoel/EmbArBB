{-# LANGUAGE CPP, 
             FlexibleContexts,
             FlexibleInstances,
             UndecidableInstances,
             MultiParamTypeClasses,
             ScopedTypeVariables #-} 


module Intel.ArBB.ReifyableType where 

import Data.Word
import Data.Int 

import Intel.ArBB.Types 
import Intel.ArBB.Vector
import Intel.ArBB.Syntax
import Intel.ArBB.Data
import Intel.ArBB.Data.Int

----------------------------------------------------------------------------
-- getting type descriptions. 

class ReifyableType a where 
    reifyType :: a -> Type

#define ReifyScalarType(a)                          \
  instance Data a => ReifyableType (Exp a)  where { \
    reifyType _ = typeOf (undefined :: a) } 

ReifyScalarType(Int)
ReifyScalarType(Int8)
ReifyScalarType(Int16)
ReifyScalarType(Int32)
ReifyScalarType(Int64)
ReifyScalarType(Float)
ReifyScalarType(Double)
ReifyScalarType(USize)
ReifyScalarType(ISize)
ReifyScalarType(Word)
ReifyScalarType(Word8)
ReifyScalarType(Word16)
ReifyScalarType(Word32)
ReifyScalarType(Word64)

--TODO: Get a better grip on (Data a, Data (Exp a))


instance Data a => ReifyableType (Exp a) where 
    reifyType a = typeOf (undefined :: a)

--instance Data (DVector t a) => ReifyableType (Exp (DVector t a)) where 
--    reifyType a = typeOf (undefined :: (DVector t a)) 

instance (ReifyableType a, ReifyableType b) => ReifyableType (a,b) where 
    reifyType (a,b) = Tuple [reifyType a, reifyType b] 

instance (ReifyableType a, ReifyableType b, ReifyableType c) => ReifyableType (a,b,c) where 
    reifyType (a,b,c) = Tuple [reifyType a, reifyType b,reifyType c] 

instance ReifyableFunType a b => ReifyableType (a -> b) where 
    reifyType f = reifyFunType f 


----------------------------------------------------------------------------
-- Function types 
class ReifyableFunType a b where 
    reifyFunType :: (a -> b) -> Type 

-- 
--instance (ReifyableType (Exp a), ReifyableType (Exp b)) 
--   => ReifyableFunType (Exp a) (Exp b) where 
--    reifyFunType f = reifyType (undefined :: Exp a) :-> 
--                     reifyType (undefined :: Exp b) 

instance (Data a, Data b)
   => ReifyableFunType (Exp a) (Exp b) where 
    reifyFunType f = typeOf (undefined :: a) :-> 
                     typeOf (undefined :: b) 


instance (ReifyableType (Exp a), ReifyableFunType b c) 
   => ReifyableFunType (Exp a) (b -> c) where 
       reifyFunType f = reifyType (undefined :: Exp a) :-> 
                        reifyFunType (f undefined)    -- Will f undefined work here ?? 
                                                      -- what problems might it cause.
                