
module Intel.ArBB.Backend.Vector where 


import Intel.ArBB.Vector (Dim)


data BEDVector d a = BEDVector { beDVectorID :: Integer
                               , beDVectorShape :: Dim}  

data BENVector a = BENVector {beNVectorID :: Integer} 




