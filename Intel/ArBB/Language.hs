{-# LANGUAGE TypeOperators #-} 

module Intel.ArBB.Language where 

import Intel.ArBB.Vector 
import Intel.ArBB.Syntax 

---------------------------------------------------------------------------- 
-- Reductions 

addReduce :: Num a => Exp (DVector (():.t) a) -> Exp (DVector t a) 
addReduce (E vec) = E $ LReduce (newLabel ()) Add vec

mulReduce :: Num a => Exp (DVector (():.t) a) -> Exp (DVector t a) 
mulReduce (E vec) = E $ LReduce (newLabel ()) Mul vec
