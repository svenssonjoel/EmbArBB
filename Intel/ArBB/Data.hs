

{-# LANGUAGE CPP, 
             FlexibleInstances, 
             FlexibleContexts,
             ScopedTypeVariables #-}
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
ScalarData(Bool,ArbbBoolean,(S.sizeOf (undefined :: Int))) -- ensure size is right

----------------------------------------------------------------------------
-- Data pairs 



instance (Data a, Data b) => Data (a,b) where 
  typeOf e = Tuple [typeOf (undefined :: a) ,typeOf (undefined :: b)] 
  sizeOf e = sizeOf (undefined :: a) + sizeOf (undefined :: b)

-- TODO: Issue here. Pairs of things are not supported very well anywhere.
--        look into that. 

