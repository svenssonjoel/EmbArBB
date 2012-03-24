{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
module Intel.ArBB.Data.Int where 

import Data.Int
import Data.Word

import Foreign.Storable
import Foreign.Ptr

import Intel.ArBB.IsScalar
import Intel.ArBB.Types

import qualified Intel.ArbbVM as VM 

newtype ISize = ISize Int64
             deriving (Eq, Show, Storable) 
newtype USize = USize Word64
             deriving (Eq, Show, Storable) 


instance Num ISize where 
  (+) (ISize a) (ISize b) = ISize (a+b)
  (-) (ISize a) (ISize b) = ISize (a-b)
  (*) (ISize a) (ISize b) = ISize (a*b)
  
  abs = undefined 
  signum = undefined 
  fromInteger = ISize . fromInteger 
  

instance Num USize where 
  (+) (USize a) (USize b) = USize (a+b)
  (-) (USize a) (USize b) = USize (a-b)
  (*) (USize a) (USize b) = USize (a*b)
  
  abs = undefined 
  signum = undefined 
  fromInteger = USize . fromInteger 
  
  
instance IsScalar USize where 
  scalarType _ = Scalar VM.ArbbUsize 
