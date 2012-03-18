
{-# LANGUAGE TypeOperators, 
             FlexibleInstances,
             ScopedTypeVariables #-}


{- 2012 Joel Svensson -} 
module Intel.ArBB.Vector where 

import qualified Data.Vector.Storable as V
import Intel.ArBB.Embeddable
import Intel.ArBB.Types 
import Intel.ArBB.Syntax 
import Intel.ArBB.IsScalar

----------------------------------------------------------------------------
-- Vectors 

data DVector d a = Vector {vectorData  :: V.Vector a, 
                           vectorShape :: Dim} 

data Dim = Zero 
         | One Int
         | Two Int Int 
         | Three Int Int Int 
         deriving Show                    

-- Encode Dimensionality in the type of vectors                    
data a :. b = a :. b  

type Dim0 = ()                
type Dim1 = () :. Dim0 
type Dim2 = () :. Dim1 
type Dim3 = () :. Dim2 


-- Easy to use names. 
type Scalar   = DVector Dim0  -- This or the next one? 
type Vector0D = DVector Dim0  -- hmm nice ?
type Vector   = DVector Dim1
type Vector2D = DVector Dim2               
type Vector3D = DVector Dim3
  
                
----------------------------------------------------------------------------
-- 

instance IsScalar a => Embeddable (DVector Dim0 a) where 
  typeOf _ = Scalar base 
    where 
      (Scalar base) = scalarType (undefined :: a) 
  
instance IsScalar a => Embeddable (DVector Dim1 a) where 
  typeOf _ = Dense I base 
    where 
      (Scalar base) = scalarType (undefined :: a) 

instance IsScalar a => Embeddable (DVector Dim2 a) where 
  typeOf _ = Dense II base 
    where 
      (Scalar base) = scalarType(undefined :: a) 

instance IsScalar a => Embeddable (DVector Dim3 a) where 
  typeOf _ = Dense III base 
    where 
      (Scalar base) = scalarType (undefined :: a) 
