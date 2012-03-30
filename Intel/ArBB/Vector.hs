
{-# LANGUAGE CPP, TypeOperators, 
             FlexibleInstances,
             ScopedTypeVariables #-}


{- 2012 Joel Svensson -} 
module Intel.ArBB.Vector where 

import qualified Data.Vector.Storable as V
import Intel.ArBB.Data -- Embeddable
import Intel.ArBB.Types 
import Intel.ArBB.Syntax 
import Intel.ArBB.IsScalar

----------------------------------------------------------------------------
-- | Zero, One, Two or Three dimensional vectors. 
-- The data payload is stored in a flat Data.Vector

data DVector d a = Vector {vectorData  :: V.Vector a, 
                           vectorShape :: Dim} 

data Dim = Zero 
         | One Int
         | Two Int Int 
         | Three Int Int Int 
         deriving Show                    

-- | Encode Dimensionality in the type of vectors                    
data a :. b = a :. b  

type Dim0 = ()                
type Dim1 = () :. Dim0 
type Dim2 = () :. Dim1 
type Dim3 = () :. Dim2 


-- | Easy to use names. 
type Scalar   = DVector Dim0  -- This or the next one? 
type Vector0D = DVector Dim0  -- hmm nice ?
type Vector   = DVector Dim1
type Vector2D = DVector Dim2               
type Vector3D = DVector Dim3
  
                
----------------------------------------------------------------------------
-- 
unS (Scalar a) = a
#define DenseOfScal(t,mod) instance IsScalar a => Data (t) where {  typeOf _ = mod (unS (scalarType (undefined :: a))); sizeOf _ = undefined}                             
DenseOfScal(DVector Dim0 a,Scalar)
DenseOfScal(DVector Dim1 a,Dense I)
DenseOfScal(DVector Dim2 a ,Dense II)
DenseOfScal(DVector Dim3 a ,Dense III)
DenseOfScal(Exp (DVector Dim0 a),Scalar)
DenseOfScal(Exp (DVector Dim1 a),Dense I)
DenseOfScal(Exp (DVector Dim2 a),Dense II)
DenseOfScal(Exp (DVector Dim3 a),Dense III)
