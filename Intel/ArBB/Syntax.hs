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

{-
{-# NOINLINE counter #-}
counter :: IORef Label
counter = unsafePerformIO (newIORef 0)


newLabel :: () -> Word32
newLabel () = unsafePerformIO $ do 
  p <- readIORef counter
  writeIORef counter (p+1)
  return p 
-}

---------------------------------------------------------------------------- 
-- Literals and Variables
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

----------------------------------------------------------------------------
--  Expression type. 
--   Now without labels. Will try to discover the sharing using the 
--   StableName method. (System.Mem.StableName)
data Expr = Lit Literal
          | Var Variable 
            
          | Index0 Expr 
            -- ArBB Functions may compute several results 
          | ResIndex Expr Int 
            
            -- Function with correct name and type must exist in some kind of environment
          | Call FunctionName [Expr]  
          | Map  FunctionName [Expr]   
            

          -- This one might need some rework! 
          --  I Will not be able to generate unique variables until 
          --  a later stage. 
          | While ([Expr] -> Expr)  ([Expr] -> [Expr])  [Expr] 
            
                 
          | If Expr Expr Expr 

          | Op Op [Expr]   
            
--          deriving (Show)

               
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
            
          | LWhile Label [Variable] LExp [LExp] [LExp]
            
                 
          | LIf  Label LExp LExp LExp 

          | LOp Label Op [LExp]   
            
          deriving (Show)

instance Eq LExp where 
  a == b = getLabel a == getLabel b
                   
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

        | Gather | Scatter | Pack | Unpack
        | Shuffle | Unshuffle | Repeat
        | Distribute | RepeatRow | RepeatCol | RepeatPage
        | Transpose | SwapCol | SwapRow | SwapPage
        | ShiftConst | ShiftClamp | ShiftConstRev | ShiftClampRev
        | Rotate | RotateRev | Reverse 
        | Length | ApplyNesting | GetNesting  
 -- TODO: The Cast needs to know what to cast to.
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

----------------------------------------------------------------------------                   
-- 

getLabel :: LExp -> Label                    
getLabel (LVar l _) = l 
getLabel (LLit l _) = l 
getLabel (LOp l _ _) = l 
getLabel (LIf l _ _ _) = l 
getLabel (LWhile l _ _ _ _) = l
getLabel (LIndex0 l _) = l 
getLabel (LResIndex l _ _) = l 
getLabel (LCall l _ _) = l
getLabel (LMap l _ _) = l

---------------------------------------------------------------------------- 
-- add a layer of types 

data Exp a = E Expr

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

