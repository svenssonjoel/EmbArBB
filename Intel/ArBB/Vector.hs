
{-# LANGUAGE TypeOperators #-} 

module Intel.ArBB.Vector where 

import qualified Data.Vector.Storable as V

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
type Vector = DVector Dim1
type Vector2D = DVector Dim2               
type Vector3D = DVector Dim3
  