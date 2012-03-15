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
-- instances 

instance Show (Exp a) where 
  show _ = "expr"

instance Eq (Exp a) where 
  (==) = undefined -- compare labels here

instance Num (Exp Int32) where 
  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ LLit (newLabel ()) $ LitInt32 $ fromInteger a


---------------------------------------------------------------------------- 
-- questionable 

instance Num (Exp (Vector0D int32)) where 
  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ LLit (newLabel ()) $ LitInt32 $ fromInteger a

