{-# LANGUAGE TypeOperators, 
             FlexibleInstances #-}

{- 2012 Joel Svensson -} 

module Intel.ArBB.Syntax where 

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
            
            -- Reduce a level in a dense container
          | LReduce Label Op LExp LExp   -- Not all Ops are valid. 
            -- Scan across a level and a given direction
          | LScan Label Op LExp LExp LExp -- Not all Ops are valid.
            
          | LRotate Label LExp LExp 
          | LRotateRev Label LExp LExp
            
            -- reverse 1D vector
          | LReverse Label LExp   
            
          | LGather Label [LExp] 
          | LScatter Label [LExp] 
            
            -- Create Constant 1D vector 
          | LConstVector Label LExp LExp 
            
            -- Concatenate 2 1D vectors
          | LCat Label LExp LExp  
            
          | LSort Label LExp LExp 
          | LSortRank Label LExp LExp -- This one has two outpus !! (how deal with this ?) 
                        
            -- ArBB Functions may compute several results 
          | LResIndex Label LExp Int 
            -- Function with correct name and type must exist in some kind of environment
          | LCall Label FunctionName [LExp]  
          | LMap  Label FunctionName [LExp]   
            
           -- Experimental  
          --  | LFor ...   
          | LIf  Label LExp LExp LExp 
            
          deriving (Show, Eq)
                   
{-
 -- reorder operations
arbb_op_gather,
arbb_op_scatter,
arbb_op_pack,
arbb_op_unpack,
arbb_op_shuffle,
arbb_op_unshuffle,
arbb_op_repeat,
arbb_op_distribute,
arbb_op_repeat_row,
arbb_op_repeat_col,
arbb_op_repeat_page,
arbb_op_transpose,
arbb_op_swap_col,
arbb_op_swap_row,
arbb_op_swap_page,
arbb_op_shift_constant,
arbb_op_shift_clamp,
arbb_op_shift_constant_reverse,
arbb_op_shift_clamp_reverse,
arbb_op_rotate,
arbb_op_rotate_reverse,
arbb_op_reverse,
arbb_op_length,
arbb_op_apply_nesting,
arbb_op_get_nesting,
arbb_op_cat,
arbb_op_cast,
arbb_op_extract,
arbb_op_split,
arbb_op_unsplit,
arbb_op_index,
arbb_op_mask,
arbb_op_copy_nesting,
arbb_op_flatten,
arbb_op_const_vector,
arbb_op_sort,
arbb_op_sort_rank,
arbb_op_replace,
arbb_op_set_regular_nesting,
arbb_op_replace_row,
arbb_op_replace_col,
arbb_op_replace_page,
arbb_op_get_nrows,
arbb_op_get_ncols,
arbb_op_get_npages,
arbb_op_extract_row,
arbb_op_extract_col,
arbb_op_extract_page,
arbb_op_section,
arbb_op_segment,
arbb_op_replace_segment,
arbb_op_alloc,
arbb_op_replace_element,
arbb_op_get_elt_coord,
arbb_op_bitwise_cast,
arbb_op_get_neighbor,
arbb_op_expect_size,
// collective operations
arbb_op_add_reduce,
arbb_op_mul_reduce,
arbb_op_max_reduce,
arbb_op_max_reduce_loc,
arbb_op_min_reduce,
arbb_op_min_reduce_loc,
arbb_op_and_reduce,
arbb_op_ior_reduce,
arbb_op_xor_reduce,
arbb_op_add_scan,
arbb_op_mul_scan,
arbb_op_max_scan,
arbb_op_min_scan,
arbb_op_and_scan,
arbb_op_ior_scan,
arbb_op_xor_scan,
arbb_op_add_merge,
arbb_op_add_merge_scalar

-}
                   
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
getLabel (LSort l _ _) = l 
getLabel (LSortRank l _ _) = l 
getLabel (LRotate l _ _) = l 
getLabel (LRotateRev l _ _) = l
getLabel (LReduce l _ _ _) = l
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

