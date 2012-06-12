{-# LANGUAGE TypeOperators, 
             FlexibleInstances,
             ExistentialQuantification, 
             DeriveDataTypeable #-}

{- 2012 Joel Svensson -} 

module Intel.ArBB.Syntax where 

--import Data.Int
--import Data.Word 

--import Intel.ArBB.Data.Int 

import Intel.ArBB.Types
import Intel.ArBB.Literal 
import Intel.ArBB.Variable
import Intel.ArBB.Op

import Intel.ArBB.MonadCapture
import Intel.ArBB.MonadBackend


import System.IO.Unsafe
import Data.IORef
import Data.Typeable

---------------------------------------------------------------------------- 
-- Literals and Variables
{- 
data Literal = LitInt    Int
             | LitInt8   Int8  
             | LitInt16  Int16
             | LitInt32  Int32
             | LitInt64  Int64
             | LitWord   Word
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
-} 
----------------------------------------------------------------------------
--  Expression type. 
--   Will try to discover the sharing using the 
--   StableName method. (System.Mem.StableName)
data Expr = Lit Literal
          | Var Variable 
            
          | Index0 Expr 
            -- ArBB Functions may compute several results 
          | ResIndex Expr Int 
            
            -- Function with correct name and type must exist in some kind of environment
          | Call FunctionName [Expr]  
          | Map  FunctionName [Expr]   

          | forall backend. MonadBackend backend => 
                NewMap (Capture backend String) [Expr]
          
          -- Hoas for the while loop.. 
          | While ([Expr] -> Expr)  ([Expr] -> [Expr])  [Expr] 
                             
          | If Expr Expr Expr -- could have been an op 

          | Op Op [Expr]   


            deriving (Typeable)
--           deriving (show)
  
----------------------------------------------------------------------------                   
-- Operations (binary, unary, reductions-ops all mixed up) 
{-
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

        | Gather | Scatter | Pack | Unpack
        | Shuffle | Unshuffle | Repeat
        | Distribute | RepeatRow | RepeatCol | RepeatPage
        | Transpose | SwapCol | SwapRow | SwapPage
        | ShiftConst | ShiftClamp | ShiftConstRev | ShiftClampRev
        | Rotate | RotateRev | Reverse 
        | Length | ApplyNesting | GetNesting  
 -- The Cast needs to know what to cast to.
        | Cat | Cast Type | Extract | Split | Unsplit
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
-} 
---------------------------------------------------------------------------- 
-- add a layer of types 

data Exp a = E {unE :: Expr}

----------------------------------------------------------------------------
-- And functions 
type FunctionName = String

data Function i o = Function FunctionName 
   deriving Show    


                 
----------------------------------------------------------------------------
data a :- b = a :- b 
infixr :- 

class ArgList a where 
  argList :: a -> [Expr] 

instance ArgList () where -- needed = 
  argList () = [] 

instance ArgList (Exp a) where 
  argList (E a) = [a] 


instance ArgList a => ArgList (Exp b :- a) where 
  argList (E b :- a) = b : argList a 

