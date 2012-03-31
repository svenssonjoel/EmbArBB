{-# language CPP #-} 
module Intel.ArBB.IsScalar where 

import Intel.ArBB.Types 
import qualified Intel.ArbbVM as VM

import Data.Int
import Data.Word

import qualified Foreign.Storable as S 

----------------------------------------------------------------------------
-- The supported basetypes 

class IsScalar a where 
  scalarType :: a -> Type 
  scalarSize :: a -> Int 
  

#define scalar(i,j,k) instance IsScalar i where { scalarType _ = Scalar VM.j; scalarSize _ = k } 
scalar(Int8,ArbbI8,1)
scalar(Int16,ArbbI16,2)
scalar(Int32,ArbbI32,4)
scalar(Int64,ArbbI64,8)
scalar(Word8,ArbbU8,1)
scalar(Word16,ArbbU16,2)
scalar(Word32,ArbbU32,4)
scalar(Word64,ArbbU64,8)
scalar(Float,ArbbF32,4) 
scalar(Double,ArbbF64,8)

instance IsScalar Int where 
  scalarType a = 
    Scalar $ case S.sizeOf a of 
               1 -> VM.ArbbI8 
               2 -> VM.ArbbI16
               4 -> VM.ArbbI32
               8 -> VM.ArbbI64
               n -> error "scalarType: <Int> Strage size"
  scalarSize a = S.sizeOf a 

instance IsScalar Word where 
  scalarType a = 
    Scalar $ case S.sizeOf a of 
               1 -> VM.ArbbU8 
               2 -> VM.ArbbU16
               4 -> VM.ArbbU32
               8 -> VM.ArbbU64
               n -> error "scalarType: <Word> Strage size"
  scalarSize a = S.sizeOf a 

