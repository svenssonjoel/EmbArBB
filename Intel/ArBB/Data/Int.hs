
module Intel.ArBB.Data.Int where 

import Data.Int
import Data.Word

data ISize = ISize Int32
             deriving (Eq, Show) 
data USize = USize Word32
             deriving (Eq, Show) 


instance Num ISize where 
  (+) (ISize a) (ISize b) = ISize (a+b)
  (-) (ISize a) (ISize b) = ISize (a-b)
  (*) (ISize a) (ISize b) = ISize (a*b)
  
  abs = undefined 
  signum = undefined 
  fromInteger = ISize . fromInteger 
  

instance Num USize where 
  (+) (USize a) (USize b) = USize (a+b)
  (-) (USize a) (USize b) = USize (a-b)
  (*) (USize a) (USize b) = USize (a*b)
  
  abs = undefined 
  signum = undefined 
  fromInteger = USize . fromInteger 
  

