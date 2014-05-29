module Intel.ArBB.Literal where 

import Data.Int
import Data.Word


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

             | LitISize  Int32 -- ISize
             | LitUSize  Word32 -- USize 
               
             | LitBool   Bool 
               deriving (Eq,Show)
