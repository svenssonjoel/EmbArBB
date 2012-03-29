{-# LANGUAGE TypeOperators, 
             FlexibleInstances #-} 

{- 2012 Joel Svensson -} 
module Intel.ArBB.Language where 

import Intel.ArBB.Vector 
import Intel.ArBB.Syntax 
import Intel.ArBB.Data.Int 

import Data.Int
import Data.Word
import Data.Bits

---------------------------------------------------------------------------- 
-- Reductions 

-- | Reduce along level 0 
addReduce0 :: Num a => Exp (DVector (():.t) a) -> Exp (DVector t a) 
addReduce0 (E vec) = E $ LReduce (newLabel ()) Add vec zero 
  where (E zero) = 0 :: Exp USize

-- | Reduce along level 0 
mulReduce0 :: Num a => Exp (DVector (():.t) a) -> Exp (DVector t a) 
mulReduce0 (E vec) = E $ LReduce (newLabel ()) Mul vec zero
  where (E zero) = 0 :: Exp USize
                                                                             
-- | reduce along a specified level 
addReduce :: Num a => Exp (DVector (():.t) a) -> Exp USize -> Exp (DVector t a) 
addReduce (E vec) (E lev) = E $ LReduce (newLabel ()) Add vec lev

-- | reduce along a specified level 
mulReduce :: Num a => Exp (DVector (():.t) a) -> Exp USize -> Exp (DVector t a) 
mulReduce (E vec) (E lev) = E $ LReduce (newLabel ()) Mul vec lev

----------------------------------------------------------------------------

-- | zero dimensional vector to scalar
index0 :: Exp (DVector () a) -> Exp a 
index0 (E vec) = E $ LIndex0 (newLabel ()) vec 

---------------------------------------------------------------------------- 
-- Scans 

-- | Scan across a specified level and direction over a dense container
addScan :: Num a 
           => (Exp (DVector t a)) 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector t a)
addScan (E vec) (E dir) (E lev) = 
  E $ LScan (newLabel ()) Add vec dir lev 

-- | Scan across a specified level and direction over a dense container
mulScan :: Num a 
           => (Exp (DVector t a)) 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector t a)
mulScan (E vec) (E dir) (E lev) = 
  E $ LScan (newLabel ()) Mul vec dir lev 



----------------------------------------------------------------------------
-- | Rotate the contents of a dense container.
-- Example: {1,2,3} -> {2,3,1}
rotate :: Exp (DVector (():.t) a) -> Exp ISize -> Exp (DVector (():.t) a) 
rotate (E vec) (E steps) = E $ LRotate (newLabel ()) vec steps  

-- | Rotate the contents of a dense container in the reverse direction
-- Example: {1,2,3} -> {3,1,2} 
rotateRev :: Exp (DVector (():.t) a) -> Exp ISize -> Exp (DVector (():.t) a) 
rotateRev (E vec) (E steps) = E $ LRotateRev (newLabel ()) vec steps  

-- | Sort the contents of a dense 1D container. Also returns 
-- a dense container of indices describing from where elements where moved
sortRank :: Exp (Vector a) -> Exp USize -> (Exp (Vector a), Exp (Vector USize)) 
sortRank (E vec) (E us)  = (fstPair s, sndPair s)
  where s = E $ LSortRank (newLabel ()) vec us

sortRank' :: Exp (Vector a) -> Exp USize -> (Exp (Vector a, Vector USize)) 
sortRank' (E vec) (E us) = E $ LSortRank (newLabel ()) vec us


-- | Sort the contents of a dense 1D container. 
sort :: Exp (Vector a) -> Exp USize -> Exp (Vector a) 
sort (E vec) (E us) = E $ LSort (newLabel ()) vec us 

----------------------------------------------------------------------------
-- | Call an ArBB Function 
call :: ArgList t => Function t (Exp r) -> t -> (Exp r) 
call (Function nom) ins = E $ LCall (newLabel ()) nom (argList ins) 


----------------------------------------------------------------------------
-- unpairing. 
fstPair :: Exp (a,b) -> Exp a 
fstPair (E a) = E $ LResIndex (newLabel ()) a 0 

sndPair :: Exp (a,b) -> Exp b
sndPair (E a) = E $ LResIndex (newLabel ()) a 1 


----------------------------------------------------------------------------
-- instances 

instance Show (Exp a) where 
  show (E a) = show a

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

instance Num (Exp Word8) where 
  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b
  (-) (E a) (E b) = E $ LBinOp (newLabel ()) Sub a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ LLit (newLabel ()) $ LitWord8 $ fromInteger a


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
  
  
----------------------------------------------------------------------------
--Minimal complete definition: .&., .|., xor, 
-- complement, (shift or (shiftL and shiftR)),  
-- (rotate or (rotateL and rotateR)), 
-- bitSize and isSigned.


instance Bits (Exp Int32) where  
  (.&.) (E a) (E b) = E $ LBinOp (newLabel ()) Bit_and a b
  (.|.) (E a) (E b) = E $ LBinOp (newLabel ()) Bit_or a b
  xor (E a) (E b) = E $ LBinOp (newLabel ()) Bit_xor a b
  
  shiftL (E a) b = E $ LBinOp (newLabel ()) Lsh a dist
   where 
     (E dist) = fromIntegral b :: (Exp Word8)   
  shiftR (E a) b = E $ LBinOp (newLabel ()) Rsh a dist
   where 
     (E dist) = fromIntegral b :: (Exp Word8)      
  complement (E a) = E $ LUnOp (newLabel ()) Bit_not a
  bitSize _ = 32   
  isSigned _ = True 