

module Intel.ArBB.Syntax where 


import Intel.ArBB.Vector 

import Data.Int
import Data.Word 

import System.IO.Unsafe
import Data.IORef
---------------------------------------------------------------------------- 
-- I think this is refered to as 'gensym'ing
type Label = Word32

{-# NOINLINE counter #-}
counter = unsafePerformIO (newIORef 0) 

newLabel :: () -> Word32
newLabel () = unsafePerformIO $ do 
  p <- readIORef counter
  writeIORef counter (p+1)
  return p 

---------------------------------------------------------------------------- 
-- 
data Literal = LitInt8   Int8  
             | LitInt32  Int32
             | LitWord32 Word32 
               deriving (Eq,Show)

data Variable = Variable String 
              deriving (Eq, Show)
               
-- Labeled Expression
data LExp = LLit Label Literal  
          | LVar Label Variable 
          | LBinOp Label Op LExp LExp
          | LUnOp Label Op LExp 
            
            -- Index into Vectors 
            -- Special case (reduction results are arrays, 
            -- sometimes of length one (a zero dimensional array
          | LIndex0 Label LExp      -- needed ? 
          | LIndex1 Label LExp LExp -- label vector index
            
            -- Operations on dense  
          | LReduce Label Op LExp   -- Not all Ops are valid. 
         -- | LAddReduce Label LExp 
         -- | LMulReduce Label LExp 
            
          | LRotate Label LExp LExp 
          | LRotateRev Label LExp LExp
            
          | LSort Label LExp 
          deriving (Show, Eq)
                   
----------------------------------------------------------------------------                   
-- Operations (binary, unary, reductions-ops all mixed up) 
data Op = Add 
        | Sub  
        | Mul 
        | Div 
          deriving (Eq, Show) 
                   
isReduceOp Add = True 
isReduceOp Mul = True
isReduceOp _   = False                    
   
----------------------------------------------------------------------------                   
-- 

getLabel :: LExp -> Label                    
getLabel (LSort l _) = l 
getLabel (LRotate l _ _) = l 
getLabel (LRotateRev l _ _) = l
getLabel (LReduce l _ _) = l
getLabel (LIndex1 l _ _) = l 
getLabel (LIndex0 l _) = l 
getLabel (LUnOp l _ _) = l 
getLabel (LBinOp l _ _ _) = l 
getLabel (LVar l _) = l 
getLabel (LLit l _) = l 

---------------------------------------------------------------------------- 
-- add a layer of types 

data Exp a = E LExp