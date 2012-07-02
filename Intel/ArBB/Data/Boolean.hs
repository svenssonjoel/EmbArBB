

module Intel.ArBB.Data.Boolean where

import Control.Monad (liftM) 

import Foreign.Ptr
import Foreign.Storable 

import Data.Int

import Intel.ArBB.IsScalar 
import Intel.ArBB.Types 
import qualified Intel.ArbbVM as VM

newtype Boolean = B {unBoolean :: Bool}
    deriving (Eq, Ord, Show) 

instance Storable Boolean where
   sizeOf _          = sizeOf (undefined::Int8)
   alignment _       = alignment (undefined::Int8)
   peekElemOff p i   = liftM B $ liftM (/= (0::Int8)) $ peekElemOff (castPtr p) i
   pokeElemOff p i x = pokeElemOff (castPtr p) i (if unBoolean x then 1 else 0::Int8)



instance IsScalar Boolean where 
    scalarType _ = Scalar VM.ArbbBoolean 
    scalarSize _ = scalarSize (undefined :: Int8) 

