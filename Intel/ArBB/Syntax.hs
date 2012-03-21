{-# LANGUAGE TypeOperators, 
             FlexibleInstances #-}

{- 2012 Joel Svensson -} 

module Intel.ArBB.Syntax where 


-- import Intel.ArBB.Vector -- why ? 

import Data.Int
import Data.Word 

import Intel.ArBB.Data.Int 

import System.IO.Unsafe
import Data.IORef
---------------------------------------------------------------------------- 
-- Label creator 
type Label = Word32

{-# NOINLINE counter #-}
counter = unsafePerformIO (newIORef 0) 

newLabel :: () -> Word32
newLabel () = unsafePerformIO $ do 
  p <- readIORef counter
  writeIORef counter (p+1)
  return p 

---------------------------------------------------------------------------- 
-- Literals and Variables
data Literal = LitInt8   Int8  
             | LitInt16  Int16
             | LitInt32  Int32
             | LitWord32 Word32 
             | LitFloat  Float 
               
             | LitDouble Double

             | LitISize  ISize
             | LitUSize  USize 
               deriving (Eq,Show)

data Variable = Variable String 
              deriving (Eq, Ord, Show)
               
---------------------------------------------------------------------------- 
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
          | LIndex2 Label LExp LExp LExp   
          | LIndex3 Label LExp LExp LExp LExp 

            -- Operations on dense  
          | LReduce Label Op LExp   -- Not all Ops are valid. 
            
          | LRotate Label LExp LExp 
          | LRotateRev Label LExp LExp
            
          | LSort Label LExp LExp 
          | LSortRank Label LExp LExp -- This one has two outpus !! (how deal with this ?) 
                        
            -- Will this work? 
            -- ArBB Functions may compute several results 
          | LResIndex Label LExp Int 
            -- Function with correct name and type must exist in some kind of environment
          | LCall Label FunctionName [LExp]  
          
          deriving (Show, Eq)
                   
-- TODO: Figure out how to get the ArBB looping constructs into the Expr 
--       datatype.


                   
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
getLabel (LSort l _ _) = l 
getLabel (LSortRank l _ _) = l 
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

----------------------------------------------------------------------------
-- And functions 
type FunctionName = String

data Function i o = Function FunctionName 

----------------------------------------------------------------------------
data a :- b = a :- b 
infixr :- 

test :: Function (Exp a :- Exp b :- Exp c :- ()) (Exp d)
test = Function "apa" 

class ArgList a where 
  argList :: a -> [LExp] 

instance ArgList () where -- needed = 
  argList () = [] 

instance ArgList (Exp a) where 
  argList (E a) = [a] 


instance ArgList a => ArgList (Exp b :- a) where 
  argList (E b :- a) = b : argList a 

