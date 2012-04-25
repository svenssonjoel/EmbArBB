

{-# LANGUAGE CPP, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module Intel.ArBB.Data where 

import Intel.ArBB.Types 
import Intel.ArBB.Syntax
import qualified Intel.ArbbVM as VM

-- The basic data types supported
import Data.Word 
import Data.Int 
import Intel.ArBB.Data.Int

import qualified Foreign.Storable as S 

class Data a where 
  typeOf :: a -> Type 
  sizeOf :: a -> Int
  

----------------------------------------------------------------------------
-- Base Data Types  
  
  
#define ScalarData(ht,arbbt,s)   \
  instance Data (ht) where    {  \
     typeOf _ = Scalar VM.arbbt; \
     sizeOf _ = s}

ScalarData(Exp Int,ArbbI64,4) -- fix for 32bit arch also! 
ScalarData(Exp Int8,ArbbI8,1) 
ScalarData(Exp Int16,ArbbI16,2) 
ScalarData(Exp Int32,ArbbI32,4) 
ScalarData(Exp Int64,ArbbI64,8) 
ScalarData(Exp Word,ArbbU64,8) -- fix for 32bit arch also! 
ScalarData(Exp Word8,ArbbU8,1) 
ScalarData(Exp Word16,ArbbU16,2) 
ScalarData(Exp Word32,ArbbU32,4) 
ScalarData(Exp Word64,ArbbU64,8)
ScalarData(Exp Float,ArbbF32,4)
ScalarData(Exp Double,ArbbF64,8)
ScalarData(Exp USize,ArbbUsize,(S.sizeOf (undefined :: Word)))
ScalarData(Exp ISize,ArbbIsize,(S.sizeOf (undefined :: Int)))

ScalarData(Int,ArbbI64,4)
ScalarData(Int8,ArbbI8,1) 
ScalarData(Int16,ArbbI16,2) 
ScalarData(Int32,ArbbI32,4) 
ScalarData(Int64,ArbbI64,8) 
ScalarData(Word,ArbbU64,8);
ScalarData(Word8,ArbbU8,1) 
ScalarData(Word16,ArbbU16,2) 
ScalarData(Word32,ArbbU32,4) 
ScalarData(Word64,ArbbU64,8)
ScalarData(Float,ArbbF32,4)
ScalarData(Double,ArbbF64,8)
ScalarData(USize,ArbbUsize,(S.sizeOf (undefined :: Word)))
ScalarData(ISize,ArbbIsize,(S.sizeOf (undefined :: Int)))

----------------------------------------------------------------------------
-- Data pairs 

instance (Data (Exp a), Data (Exp b)) => Data (Exp a, Exp b) where 
  typeOf (e1,e2) = Tuple [typeOf e1,typeOf e2] 
  sizeOf (e1,e2) = sizeOf e1 + sizeOf e2
  
instance (Data a, Data b) => Data (Exp (a, b)) where 
  typeOf e = Tuple [typeOf (undefined :: a), typeOf (undefined :: b)] 
  sizeOf e = sizeOf (undefined :: a) + sizeOf (undefined :: b)