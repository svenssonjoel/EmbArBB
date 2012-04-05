{-# LANGUAGE TypeOperators, 
             FlexibleInstances #-}

{- 2012 Joel Svensson -} 

module Intel.ArBB.Syntax where 

import Data.Int
import Data.Word 

import Intel.ArBB.Data.Int 
import Intel.ArBB.Types

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
             | LitInt64  Int64
             | LitWord8  Word8
             | LitWord16 Word16
             | LitWord32 Word32 
             | LitWord64 Word64  
             | LitFloat  Float 
             | LitDouble Double

             | LitISize  ISize
             | LitUSize  USize 
               
             | LitBool   Bool 
               deriving (Eq,Show)

data Variable = Variable String 
              deriving (Eq, Ord, Show)
               
---------------------------------------------------------------------------- 
-- Labeled Expression
data LExp = LLit Label Literal  
          | LVar Label Variable 
            
          | LIndex0 Label LExp 
     
            -- ArBB Functions may compute several results 
          | LResIndex Label LExp Int 
            -- Function with correct name and type must exist in some kind of environment
          | LCall Label FunctionName [LExp]  
          | LMap  Label FunctionName [LExp]   
            
          -- Experimental  
          | LFor Label ([LExp] -> LExp)   -- loop condition 
                       ([LExp] -> [LExp]) -- loop body 
                       [LExp]             -- start state
          | LFor' Label [(Variable,Type)] LExp [LExp] [LExp]

                 
          | LIf  Label LExp LExp LExp 
            
            -- Experiment: Encode all ArBBOps in this constr
          | LOp Label Op [LExp]   
            
          deriving (Show)
                   
instance Show (a->b) where 
  show _ = "func" 

instance Eq LExp where 
  a == b = getLabel a == getLabel b

 
-- TODO: Figure out how to get the ArBB looping constructs into the Expr 
--       datatype.


                   
----------------------------------------------------------------------------                   
-- Operations (binary, unary, reductions-ops all mixed up) 
data Op = Add | Sub  | Mul | Div 
        | Max | Min
        | And | Ior | Xor         
        | Abs | Acos | Asin | Atan | Ceil
        | Cos | Cosh | Exp | Exp10
        | Floor | Ln | Log10 | Log_not
        | Bit_not | Rcp | Round | Rsqrt
        | Sin | Sinh | Sqrt | Tan | Tanh
        | Neg | Bit_and | Atan2
        | Compare | Equal
        | Geq | Greater | Bit_or
        | Leq | Less | Log_and | Log_or 
        | Lsh | Mod 
        | Neq | Pow | Rsh 
        | Bit_xor | Select
         -- Experimental
        | Gather | Scatter | Pack | Unpack
        | Shuffle | Unshuffle | Repeat
        | Distribute | RepeatRow | RepeatCol | RepeatPage
        | Transpose | SwapCol | SwapRow | SwapPage
        | ShiftConst | ShiftClamp | ShiftConstRev | ShiftClampRev
        | Rotate | RotateRev | Reverse 
        | Length | ApplyNesting | GetNesting  
        | Cat | Cast | Extract | Split | Unsplit
        | Index | Mask | CopyNesting | Flatten 
        | ConstVector | Sort | SortRank | Replace 
        | SetRegularNesting | ReplaceRow | ReplaceCol | ReplacePage 
        | GetNRows | GetNCols | GetNPages 
        | ExtractRow | ExtractCol | ExtractPage 
        | Section | Segment | ReplaceSegment
        | Alloc | ReplaceElem 
        | GetEltCoord | BitwiseCast
        | GetNeighbor | ExpectSize 
        | AddReduce | MulReduce | MaxReduce | MaxReduceLoc 
        | MinReduce | MinReduceLoc 
        | AndReduce | IorReduce | XorReduce 
        | AddScan | MulScan | MaxScan | MinScan | AndScan 
        | IorScan | XorScan 
        | AddMerge | AddMergeScalar
                     
          deriving (Eq, Show) 
                   
isReduceOp Add = True 
isReduceOp Mul = True
isReduceOp Max = True
isReduceOp Min = True 
isReduceOp And = True
isReduceOp Ior = True 
isReduceOp Xor = True
isReduceOp _   = False                    
   
isScanOp Add = True 
isScanOp Mul = True
isScanOp Max = True
isScanOp Min = True 
isScanOp And = True
isScanOp Ior = True 
isScanOp Xor = True
isScanOp _   = False                    

----------------------------------------------------------------------------                   
-- 

getLabel :: LExp -> Label                    
getLabel (LVar l _) = l 
getLabel (LLit l _) = l 
getLabel (LOp l _ _) = l 
getLabel (LIf l _ _ _) = l 
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

