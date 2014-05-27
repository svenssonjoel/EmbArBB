module Intel.ArBB.Op where

import Intel.ArBB.Types

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
 -- The Cast needs to know what to cast to.
        | Cat | Cast Type | Extract | Split | Unsplit
        | Index | Mask | CopyNesting | Flatten 
        | ConstVector | Sort | SortRank | Replace 
        | SetRegularNesting | ReplaceRow | ReplaceCol | ReplacePage 
        | GetNRows | GetNCols | GetNPages 
        | ExtractRow | ExtractCol | ExtractPage 
        | Section | Segment | ReplaceSegment
        | Alloc | ReplaceElem 
        | GetEltCoord | BitwiseCast Type
        | GetNeighbor | ExpectSize 
        | AddReduce | MulReduce | MaxReduce | MaxReduceLoc 
        | MinReduce | MinReduceLoc 
        | AndReduce | IorReduce | XorReduce 
        | AddScan | MulScan | MaxScan | MinScan | AndScan 
        | IorScan | XorScan 
        | AddMerge | AddMergeScalar
          deriving (Eq, Show, Read, Ord) 
