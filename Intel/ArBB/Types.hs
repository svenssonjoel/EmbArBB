{- 2012 Joel Svensson -} 

module Intel.ArBB.Types where 

--import qualified Intel.ArbbVM as VM
--import qualified Intel.ArbbVM.Convenience as VM 

---------------------------------------------------------------------------- 
--
data ScalarType = I8 | I16 | I32 | I64
                | U8 | U16 | U32 | U64
                | F32 | F64
                | Boolean
                | USize
                | ISize
                 deriving (Eq,Show,Read,Ord)

data Type = Scalar ScalarType 
          | Dense Dimensionality ScalarType 
          | Nested ScalarType
                        
          -- Tuple types 
          | Tuple [Type] 
-- EXPERIMENTAL function types
          | Type :-> Type
          deriving (Eq,Show,Read,Ord)                  
infixr :-> 

data Dimensionality = I | II | III 
                    deriving (Eq, Show, Read, Ord)
---------------------------------------------------------------------------
-- Size in bytes of an element of a given type.
size :: ScalarType -> Int
size I8  = 1
size I16 = 2 
size I32 = 4
size I64 = 8 
size U8  = 1 
size U16 = 2 
size U32 = 4 
size U64 = 8
size F32 = 4
size F64 = 8 
size Boolean = 4 
size USize = 4  
size ISize = 4  


---------------------------------------------------------------------------- 
--  Type  helpers
isScalar :: Type -> Bool 
isScalar (Scalar _) = True
isScalar _ = False 

is1D :: Type -> Bool 
is1D (Dense I _) = True 
is1D _ = False 

is2D (Dense II _) = True 
is2D _ = False 

is3D (Dense III _) = True
is3D _ = False

decrRank (Dense III a) = Just$ Dense II a 
decrRank (Dense II a) = Just$ Dense I a 
decrRank (Dense I a) = Just$ Scalar a 
decrRank a = error $ "decrRank on :" ++ show a  -- Nothing 

container :: Type -> (ScalarType -> Type) 
container (Dense t a) b = Dense t b 
container _ _ = error "container: not a container" 

----------------------------------------------------------------------------
-- get input/output types from a function type 
getInputTypes :: Type -> [Type] 
getInputTypes t@(a :-> b) = getInputTypes' t 
    where 
      getInputTypes' (a :-> b) = a : getInputTypes' b
      getInputTypes' _ = []
getInputTypes _ = error "getInputTypes: not a function type" 

getOutputType :: Type -> Type 
getOutputType (a :-> b) = getOutputType' b
    where 
      getOutputType' (a :-> b) = getOutputType' b
      getOutputType' b         = b 
getOutputType _         = error "getOutputTypes: not a function type" 

