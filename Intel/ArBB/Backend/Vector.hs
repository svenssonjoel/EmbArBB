
module Intel.ArBB.Backend.Vector where 


import Intel.ArBB.Vector (Dim1)
import Intel.ArBB.Syntax (Dim)

--import Intel.ArBB.Data.Int
import Data.Word

data BEDVector d a = BEDVector { beDVectorID :: Integer
                               , beDVectorShape :: d}  

data BENVector a = BENVector { beNVectorID :: Integer
                             , beNVectorData :: BEDVector Dim1 a
                             , beNVectorNest :: BEDVector Dim1 Word32 }





