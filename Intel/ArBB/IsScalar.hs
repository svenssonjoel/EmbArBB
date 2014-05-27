{-# language CPP #-} 
module Intel.ArBB.IsScalar where 

import Intel.ArBB.Types 
-- import qualified Intel.ArbbVM as VM

import Data.Int
import Data.Word

import qualified Foreign.Storable as S 

----------------------------------------------------------------------------
-- The supported basetypes 

class IsScalar a where 
  scalarType :: a -> Type 
  scalarSize :: a -> Int 
  

#define scalar(i,j,k)           \
  instance IsScalar i where {   \
    scalarType _ = Scalar j; \
    scalarSize _ = k }

scalar(Int8,I8,1)
scalar(Int16,I16,2)
scalar(Int32,I32,4)
scalar(Int64,I64,8)
scalar(Word8,U8,1)
scalar(Word16,U16,2)
scalar(Word32,U32,4)
scalar(Word64,U64,8)
scalar(Float,F32,4) 
scalar(Double,F64,8)

instance IsScalar Int where 
  scalarType a = 
    Scalar $ case S.sizeOf a of 
               1 -> I8 
               2 -> I16
               4 -> I32
               8 -> I64
               n -> error "scalarType: <Int> Strage size"
  scalarSize a = S.sizeOf a 

instance IsScalar Word where 
  scalarType a = 
    Scalar $ case S.sizeOf a of 
               1 -> U8 
               2 -> U16
               4 -> U32
               8 -> U64
               n -> error "scalarType: <Word> Strage size"
  scalarSize a = S.sizeOf a 

-- Should this be Boolean ? 
instance IsScalar Bool where 
  scalarType a = 
    Scalar Boolean
  scalarSize a = S.sizeOf (undefined :: Int)
