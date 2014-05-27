

{-# LANGUAGE CPP #-}  
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Intel.ArBB.Data where 

import Intel.ArBB.Types as T
import Intel.ArBB.Syntax
-- import qualified Intel.ArbbVM as VM

-- The basic data types supported
import Data.Word 
import Data.Int 
import Intel.ArBB.Data.Int
import Intel.ArBB.Data.Boolean

import qualified Foreign.Storable as S 

class Data a where 
  typeOf :: a -> Type 
  sizeOf :: a -> Int
  

----------------------------------------------------------------------------
-- Base Data Types  
  

#define ScalarData(ht,arbbt,s)   \
  instance Data (ht) where    {  \
     typeOf _ = Scalar arbbt; \
     sizeOf _ = s}



ScalarData(Int,I64,4)
ScalarData(Int8,I8,1) 
ScalarData(Int16,I16,2) 
ScalarData(Int32,I32,4) 
ScalarData(Int64,I64,8) 
ScalarData(Word,U64,8);
ScalarData(Word8,U8,1) 
ScalarData(Word16,U16,2) 
ScalarData(Word32,U32,4) 
ScalarData(Word64,U64,8)
ScalarData(Float,F32,4)
ScalarData(Double,F64,8)
ScalarData(USize,T.USize,(S.sizeOf (undefined :: Word)))
ScalarData(ISize,T.ISize,(S.sizeOf (undefined :: Int)))
ScalarData(Boolean,Boolean,4) -- ensure size is right

----------------------------------------------------------------------------
-- Data pairs 



instance (Data a, Data b) => Data (a,b) where 
  typeOf e = Tuple [typeOf (undefined :: a) ,typeOf (undefined :: b)] 
  sizeOf e = sizeOf (undefined :: a) + sizeOf (undefined :: b)

-- TODO: Issue here. Pairs of things are not supported very well anywhere.
--        look into that. 

