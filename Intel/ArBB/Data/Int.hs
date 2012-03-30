{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
module Intel.ArBB.Data.Int where 

import Data.Int
import Data.Word

import Foreign.Storable
import Foreign.Ptr

import Intel.ArBB.IsScalar
import Intel.ArBB.Types

import qualified Intel.ArbbVM as VM 

newtype ISize = ISize Int    
             deriving (Eq, Ord, Show, Storable, Real, Enum, Num, Integral ) 
newtype USize = USize Word
             deriving (Eq, Ord, Show, Storable, Real, Enum, Num, Integral ) 


instance IsScalar USize where 
  scalarType _ = Scalar VM.ArbbUsize 
  scalarSize _ = scalarSize (undefined :: Word) 

instance IsScalar ISize where 
  scalarType _ = Scalar VM.ArbbIsize 
  scalarSize _ = scalarSize (undefined :: Int)
