{-# LANGUAGE TypeOperators, 
             FlexibleInstances #-} 

{- 2012 Joel Svensson -} 
module Intel.ArBB.Language where 

import Intel.ArBB.Vector 
import Intel.ArBB.Syntax 
import Intel.ArBB.Data.Int 

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
rotate :: Exp (DVector (():.t) a) -> Exp ISize -> Exp (DVector (():.t) a) 
rotate (E vec) (E steps) = E $ LRotate (newLabel ()) vec steps  

rotateRev :: Exp (DVector (():.t) a) -> Exp ISize -> Exp (DVector (():.t) a) 
rotateRev (E vec) (E steps) = E $ LRotateRev (newLabel ()) vec steps  


sortRank :: Exp (Vector a) -> Exp USize -> Exp (Vector a, Vector USize) 
sortRank (E vec) (E us@(LLit _ (LitUSize l)))  = E $ LSortRank (newLabel ()) vec us


----------------------------------------------------------------------------
-- Function calling 

call :: ArgList t => Function t (Exp r) -> t -> (Exp r) 
call (Function nom) ins = E $ LCall (newLabel ()) nom (argList ins) 

-- TODO: Improve (types ???) but how !!
resIndex :: Exp a -> Int -> Exp b 
resIndex (E a) i = E $ LResIndex (newLabel ()) a i 

fstPair :: Exp (a,b) -> Exp a 
fstPair (E a) = E $ LResIndex (newLabel ()) a 0 

sndPair :: Exp (a,b) -> Exp b
sndPair (E a) = E $ LResIndex (newLabel ()) a 1 


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

instance Num (Exp Float) where 
  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b
  (-) (E a) (E b) = E $ LBinOp (newLabel ()) Sub a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ LLit (newLabel ()) $ LitFloat $ fromInteger a


instance Num (Exp Word32) where 
  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b
  (-) (E a) (E b) = E $ LBinOp (newLabel ()) Sub a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ LLit (newLabel ()) $ LitWord32 $ fromInteger a
  
instance Num (Exp ISize) where 
  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b
  (-) (E a) (E b) = E $ LBinOp (newLabel ()) Sub a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ LLit (newLabel ()) $ LitISize $ fromInteger a
  
  
instance Num (Exp USize) where 
  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b
  (-) (E a) (E b) = E $ LBinOp (newLabel ()) Sub a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ LLit (newLabel ()) $ LitUSize $ fromInteger a


----------------------------------------------------------------------------
-- 
instance Num a => Num (Exp (DVector t a)) where 
  (+) (E v1) (E v2) = E $ LBinOp (newLabel ()) Add v1 v2 
  (*) (E v1) (E v2) = E $ LBinOp (newLabel ()) Mul v1 v2 
  (-) (E v1) (E v2) = E $ LBinOp (newLabel ()) Sub v1 v2 
  
  abs = undefined 
  signum = undefined 
  
  fromInteger = undefined 