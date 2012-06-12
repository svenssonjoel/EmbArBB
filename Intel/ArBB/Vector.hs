
{-# LANGUAGE CPP, TypeOperators, 
             FlexibleInstances,
             ScopedTypeVariables #-}


{- 2012 Joel Svensson -} 
module Intel.ArBB.Vector where 

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M 
import Intel.ArBB.Data -- Embeddable
import Intel.ArBB.Types 
import Intel.ArBB.Syntax 
import Intel.ArBB.IsScalar


----------------------------------------------------------------------------
-- Dense Vectors
data DVector d a = DVector {dVectorID :: Integer, 
                            dVectorShape :: Dim}

data Dim = Dim {dimList :: [Int]}

dimensions = length . dimList

class Dimensions a where 
    toDim :: a -> Dim 
instance Dimensions Z where 
    toDim Z = Dim []

instance Dimensions t => Dimensions (Int :. t) where  
    toDim (i :. is) = Dim (i:is')
        where (Dim is') = toDim is

-- | Encode Dimensionality in the type of vectors                    
data a :. b = a :. b  
infixr :. 

data Z = Z


type Dim0 = Z             
type Dim1 = Int :. Dim0 
type Dim2 = Int :. Dim1 
type Dim3 = Int :. Dim2 


-- | Easy to use names. 
type Scalar   = DVector Dim0  -- This or the next one? 
type Vector0D = DVector Dim0  -- hmm nice ?
type Vector   = DVector Dim1
type Vector2D = DVector Dim2               
type Vector3D = DVector Dim3
  



----------------------------------------------------------------------------
-- Nested Vectors 
data NVector = NVector {nVectorID :: Integer}
                        

----------------------------------------------------------------------------


{- 
data DVector d a = Vector {vectorData  :: V.Vector a, 
                           vectorShape :: Dim} 
                 deriving Show

data Dim = Zero 
         | One Int
         | Two Int Int 
         | Three Int Int Int 
         deriving Show                    


----------------------------------------------------------------------------
-- 
-} 
unS (Scalar a) = a
#define DenseOfScal(t,mod) instance IsScalar a => Data (t) where { \
  typeOf _ = mod (unS (scalarType (undefined :: a)));              \
  sizeOf _ = undefined}                             

DenseOfScal(DVector Dim0 a,Scalar)
DenseOfScal(DVector Dim1 a,Dense I)
DenseOfScal(DVector Dim2 a ,Dense II)
DenseOfScal(DVector Dim3 a ,Dense III)
DenseOfScal(Exp (DVector Dim0 a),Scalar)
DenseOfScal(Exp (DVector Dim1 a),Dense I)
DenseOfScal(Exp (DVector Dim2 a),Dense II)
DenseOfScal(Exp (DVector Dim3 a),Dense III)
{-
----------------------------------------------------------------------------
-- Mutable Vectors 

-- Figure out how to use these.. and how to handle that s parameter... 
-- TODO: IOVector ?
data MDVector d a = MVector {mVectorData  :: M.IOVector a, 
                             mVectorShape :: Dim} 
                    
                    
freeze :: M.Storable a => MDVector d a -> IO (DVector d a) 
freeze (MVector dat dim) = 
  do 
    dat' <- V.freeze dat 
    return $ Vector dat' dim

    


new1D :: M.Storable a => Int -> IO (MDVector Dim1 a) 
new1D n = 
  do 
   vec <- M.new n 
   return $ MVector vec (One n)
   
new2D :: M.Storable a => Int -> Int -> IO (MDVector Dim2 a) 
new2D n1 n2 = 
  do 
    vec <- M.new (n1*n2) 
    return $ MVector vec (Two n1 n2)
    
new3D :: M.Storable a => Int -> Int -> Int -> IO (MDVector Dim3 a) 
new3D n1 n2 n3 = 
  do 
    vec <- M.new (n1*n2*n3) 
    return $ MVector vec (Three n1 n2 n3)    
-}     
    