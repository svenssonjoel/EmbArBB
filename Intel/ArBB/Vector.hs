
{-# LANGUAGE CPP, TypeOperators, 
             FlexibleInstances,
             ScopedTypeVariables,
             MultiParamTypeClasses,
             KindSignatures #-}


{- 2012 Joel Svensson -} 
module Intel.ArBB.Vector where 

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M 
import Intel.ArBB.Data -- Embeddable
import Intel.ArBB.Data.Int 
import Intel.ArBB.Types 
import Intel.ArBB.Syntax 
import Intel.ArBB.IsScalar


----------------------------------------------------------------------------
-- Dense Vectors
data DVector d a = DVector { dVectorData  :: V.Vector a  -- dVectorID :: Integer, 
                           , dVectorShape :: Dim}

mkDVector :: Dimensions t => V.Vector a -> t -> DVector t a 
mkDVector v d = DVector v (toDim d)  

data Dim = Dim {dimList :: [Int]}

dimensions = length . dimList

-- In ArBB the order seems to be Pages, Rows, Cols.  
-- so                            Z:.Cols:.Rows:.Pages  in EmbArBB.
-- Does that make sense ? 

class Dimensions a where 
    toDim :: a -> Dim 
instance Dimensions Z where 
    toDim Z = Dim []

instance Dimensions t => Dimensions (t :. Int) where  
    toDim (is :. i) = Dim (i:is')
        where (Dim is') = toDim is

-- | Encode Dimensionality in the type of vectors                    
data a :. b = a :. b  
infixl :. 

data Z = Z


type Dim0 = Z             
type Dim1 = Dim0 :. Int
type Dim2 = Dim1 :. Int 
type Dim3 = Dim2 :. Int 


-- | Easy to use names. 
--type Scalar   = DVector Dim0  -- This or the next one? 
type Vector0D = DVector Dim0  -- hmm nice ?
type Vector   = DVector Dim1
type Vector2D = DVector Dim2               
type Vector3D = DVector Dim3
  
----------------------------------------------------------------------------
-- Nested Vectors 
data NVector a = NVector { nVectorData     :: V.Vector a
                         , nVectorNesting  :: V.Vector USize
                         , nVectorSize     :: Int
                         , nVectorSegs     :: Int}


----------------------------------------------------------------------------
-- Data Instances.. 
unS (Scalar a) = a
#define ContainerOfScal(t,mod) instance IsScalar a => Data (t) where { \
  typeOf _ = mod (unS (scalarType (undefined :: a)));              \
  sizeOf _ = undefined}                             

ContainerOfScal(DVector Dim0 a,Scalar)
ContainerOfScal(DVector Dim1 a,Dense I)
ContainerOfScal(DVector Dim2 a ,Dense II)
ContainerOfScal(DVector Dim3 a ,Dense III)

ContainerOfScal(NVector a, Nested) 


----------------------------------------------------------------------------
-- IsVector 

class IsVector (t :: * -> *)  e  

instance IsScalar a => IsVector NVector a 

-- Zero dimensional DVectors are not Vectors. 
instance (Dimensions t, IsScalar a) => IsVector (DVector (t:.Int)) a 
    
    

----------------------------------------------------------------------------
-- Show 

instance (M.Storable a, Show a) => Show (DVector t a) where 
    show (DVector dat t) = show dat
-- TODO: show based on the dimensions. (so a matrix appears as a matrix)