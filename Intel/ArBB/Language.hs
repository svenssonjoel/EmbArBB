{-# LANGUAGE TypeOperators, 
             FlexibleInstances #-} 

module Intel.ArBB.Language where 

import Intel.ArBB.Vector 
import Intel.ArBB.Syntax 

import Data.Int
import Data.Word

---------------------------------------------------------------------------- 
-- Reductions 

addReduce :: Num a => Exp (DVector (():.t) a) -> Exp (DVector t a) 
addReduce (E vec) = E $ LReduce (newLabel ()) Add vec

mulReduce :: Num a => Exp (DVector (():.t) a) -> Exp (DVector t a) 
mulReduce (E vec) = E $ LReduce (newLabel ()) Mul vec


----------------------------------------------------------------------------
-- Rotate arrays (example:  [1,2,3] -> [2,3,1]) 
rotate :: Exp (DVector (():.t) a) -> Exp Word32 -> Exp (DVector (():.t) a) 
rotate (E vec) (E steps) = E $ LRotate (newLabel ()) vec steps  

rotateRev :: Exp (DVector (():.t) a) -> Exp Word32 -> Exp (DVector (():.t) a) 
rotateRev (E vec) (E steps) = E $ LRotateRev (newLabel ()) vec steps  





----------------------------------------------------------------------------
-- instances 

instance Show (Exp a) where 
  show _ = "expr"

instance Eq (Exp a) where 
  (==) = undefined -- compare labels here

instance Num (Exp Int32) where 
  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b
  (-) (E a) (E b) = E $ LBinOp (newLabel ()) Sub a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ LLit (newLabel ()) $ LitInt32 $ fromInteger a


instance Num (Exp Word32) where 
  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b
  (-) (E a) (E b) = E $ LBinOp (newLabel ()) Sub a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ LLit (newLabel ()) $ LitWord32 $ fromInteger a

---------------------------------------------------------------------------- 
-- questionable 

--instance Num (Exp (Vector0D int32)) where 
--  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
--  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b
--  (-) (E a) (E b) = E $ LBinOp (newLabel ()) Sub a b

--  abs = undefined 
--  signum = undefined 
  
--  fromInteger a = E $ LLit (newLabel ()) $ LitInt32 $ fromInteger a

----------------------------------------------------------------------------
-- 
instance Num a => Num (Exp (DVector t a)) where 
  (+) (E v1) (E v2) = E $ LBinOp (newLabel ()) Add v1 v2 
  (*) (E v1) (E v2) = E $ LBinOp (newLabel ()) Mul v1 v2 
  (-) (E v1) (E v2) = E $ LBinOp (newLabel ()) Sub v1 v2 
  
  abs = undefined 
  signum = undefined 
  
  fromInteger = undefined 