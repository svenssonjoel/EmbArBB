{-# LANGUAGE TypeOperators, 
             FlexibleInstances, 
             FlexibleContexts, 
             UndecidableInstances,
             ScopedTypeVariables #-} 

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
addReduce0 (E vec) = 
  E $ LOp (newLabel ()) AddReduce [vec,zero] 
  where (E zero) = 0 :: Exp USize

-- | Reduce along level 0 
mulReduce0 :: Num a => Exp (DVector (():.t) a) -> Exp (DVector t a) 
mulReduce0 (E vec) = 
  E $ LOp (newLabel ()) MulReduce [vec,zero]
  where (E zero) = 0 :: Exp USize
                                                                             
-- | reduce along a specified level 
addReduce :: Num a => Exp (DVector (():.t) a) -> Exp USize -> Exp (DVector t a) 
addReduce (E vec) (E lev) = 
  E $ LOp (newLabel ()) AddReduce [vec,lev]

-- | reduce along a specified level 
mulReduce :: Num a => Exp (DVector (():.t) a) -> Exp USize -> Exp (DVector t a) 
mulReduce (E vec) (E lev) = 
  E $ LOp (newLabel ()) MulReduce [vec,lev]

-- | reduce along a specified level 
maxReduce :: Num a => Exp (DVector (():.t) a) -> Exp USize -> Exp (DVector t a) 
maxReduce (E vec) (E lev) = 
  E $ LOp (newLabel ()) MaxReduce [vec,lev]

-- | reduce along a specified level 
minReduce :: Num a => Exp (DVector (():.t) a) -> Exp USize -> Exp (DVector t a) 
minReduce (E vec) (E lev) = 
  E $ LOp (newLabel ()) MinReduce [vec,lev]

-- | reduce along a specified level 
andReduce :: Exp (DVector (():.t) Bool) -> Exp USize -> Exp (DVector t Bool) 
andReduce (E vec) (E lev) = 
  E $ LOp (newLabel ()) AndReduce [vec,lev]

-- | reduce along a specified level 
iorReduce :: Exp (DVector (():.t) Bool) -> Exp USize -> Exp (DVector t Bool) 
iorReduce (E vec) (E lev) = 
  E $ LOp (newLabel ()) IorReduce [vec,lev]

-- | reduce along a specified level 
xorReduce :: Exp (DVector (():.t) Bool) -> Exp USize -> Exp (DVector t Bool) 
xorReduce (E vec) (E lev) = 
  E $ LOp (newLabel ()) XorReduce [vec,lev]

----------------------------------------------------------------------------

-- | zero dimensional vector to scalar
index0 :: Exp (DVector () a) -> Exp a 
index0 (E vec) = E $ LIndex0 (newLabel ()) vec 

-- | Index into a 1D vector
index1 :: Exp (DVector Dim1 a) -> Exp USize -> Exp a 
index1 (E vec) (E ix) = 
  E $ LOp (newLabel ()) Extract [vec,ix] 

-- | Index into a 2D vector
index2 :: Exp (DVector Dim2 a) -> Exp USize -> Exp USize -> Exp a 
index2 (E vec) (E ix1) (E ix2)  = 
  E $ LOp (newLabel ()) Extract [vec,ix1,ix2]  

-- | Index into a 3D vector 
index3 :: Exp (DVector Dim2 a) -> Exp USize -> Exp USize -> Exp USize -> Exp a 
index3 (E vec) (E ix1) (E ix2) (E ix3) = 
  E $ LOp (newLabel ()) Extract [vec,ix1,ix2,ix3] 


---------------------------------------------------------------------------- 
-- Scans 

-- | Scan across a specified level and direction over a dense container
addScan :: Num a 
           => (Exp (DVector t a)) 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector t a)
addScan (E vec) (E dir) (E lev) = 
  E $ LOp (newLabel ()) AddScan [vec,dir,lev] 


-- | Scan across a specified level and direction over a dense container
mulScan :: Num a 
           => (Exp (DVector t a)) 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector t a)
mulScan (E vec) (E dir) (E lev) = 
  E $ LOp (newLabel ()) MulScan [vec,dir,lev] 

-- | Scan across a specified level and direction over a dense container
maxScan :: Num a 
           => (Exp (DVector t a)) 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector t a)
maxScan (E vec) (E dir) (E lev) = 
  E $ LOp (newLabel ()) MaxScan [vec,dir,lev] 
  
-- | Scan across a specified level and direction over a dense container
minScan :: Num a 
           => (Exp (DVector t a)) 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector t a)
minScan (E vec) (E dir) (E lev) = 
  E $ LOp (newLabel ()) MinScan [vec,dir,lev] 
  

-- | Scan across a specified level and direction over a dense container
andScan :: (Exp (DVector t Bool)) 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector t Bool)
andScan (E vec) (E dir) (E lev) = 
  E $ LOp (newLabel ()) AndScan [vec,dir,lev] 
  
-- | Scan across a specified level and direction over a dense container
iorScan ::  (Exp (DVector t Bool)) 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector t Bool)
iorScan (E vec) (E dir) (E lev) = 
  E $ LOp (newLabel ()) IorScan [vec,dir,lev] 
  
-- | Scan across a specified level and direction over a dense container
xorScan ::  (Exp (DVector t Bool)) 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector t Bool)
xorScan (E vec) (E dir) (E lev) = 
  E $ LOp (newLabel ()) XorScan [vec,dir,lev] 

  
  
----------------------------------------------------------------------------
-- | Rotate the contents of a dense container.
-- Example: {1,2,3} -> {2,3,1}
rotate :: Exp (DVector (():.t) a) -> Exp ISize -> Exp (DVector (():.t) a) 
rotate (E vec) (E steps) = 
  E $ LOp (newLabel ()) Rotate [vec,steps]  

-- | Rotate the contents of a dense container in the reversedirection
-- Example: {1,2,3} -> {3,1,2} 
rotateRev :: Exp (DVector (():.t) a) -> Exp ISize -> Exp (DVector (():.t) a) 
rotateRev (E vec) (E steps) = 
  E $ LOp (newLabel ()) RotateRev [vec,steps]  

-- | Reverse a 1D vector 
reverse :: Exp (Vector a) -> Exp (Vector a) 
reverse (E vec) = E $ LOp (newLabel ()) Reverse [vec]

-- | Sort the contents of a dense 1D container. Also returns 
-- a dense container of indices describing from where elements where moved
sortRank :: Exp (Vector a) -> Exp USize -> (Exp (Vector a), Exp (Vector USize)) 
sortRank (E vec) (E us)  = (fstPair s, sndPair s)
  where s = E $ LOp (newLabel ()) SortRank [vec,us]

sortRank' :: Exp (Vector a) -> Exp USize -> (Exp (Vector a, Vector USize)) 
sortRank' (E vec) (E us) = 
  E $ LOp (newLabel ()) SortRank [vec,us]


-- | Sort the contents of a dense 1D container. 
sort :: Exp (Vector a) -> Exp USize -> Exp (Vector a) 
sort (E vec) (E us) = 
  E $ LOp (newLabel ()) Sort [vec,us] 

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
-- conditional 

ifThenElse :: (Exp Bool) -> (Exp a) -> (Exp a) -> (Exp a)
ifThenElse (E b) (E e1) (E e2) = E $ LIf (newLabel ()) b e1 e2


----------------------------------------------------------------------------
-- instances 

instance Show (Exp a) where 
  show (E a) = show a

instance Eq (Exp a) where 
  (==) = undefined -- compare labels here

instance Num (Exp Int8) where 
  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b
  (-) (E a) (E b) = E $ LBinOp (newLabel ()) Sub a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ LLit (newLabel ()) $ LitInt8 $ fromInteger a

instance Num (Exp Int16) where 
  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b
  (-) (E a) (E b) = E $ LBinOp (newLabel ()) Sub a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ LLit (newLabel ()) $ LitInt16 $ fromInteger a

instance Num (Exp Int32) where 
  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b
  (-) (E a) (E b) = E $ LBinOp (newLabel ()) Sub a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ LLit (newLabel ()) $ LitInt32 $ fromInteger a

instance Num (Exp Int64) where 
  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b
  (-) (E a) (E b) = E $ LBinOp (newLabel ()) Sub a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ LLit (newLabel ()) $ LitInt64 $ fromInteger a

instance Num (Exp Word8) where 
  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b
  (-) (E a) (E b) = E $ LBinOp (newLabel ()) Sub a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ LLit (newLabel ()) $ LitWord8 $ fromInteger a

instance Num (Exp Word16) where 
  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b
  (-) (E a) (E b) = E $ LBinOp (newLabel ()) Sub a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ LLit (newLabel ()) $ LitWord16 $ fromInteger a


instance Num (Exp Word32) where 
  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b
  (-) (E a) (E b) = E $ LBinOp (newLabel ()) Sub a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ LLit (newLabel ()) $ LitWord32 $ fromInteger a
  
instance Num (Exp Word64) where 
  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b
  (-) (E a) (E b) = E $ LBinOp (newLabel ()) Sub a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ LLit (newLabel ()) $ LitWord64 $ fromInteger a


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


instance Num (Exp Float) where 
  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b
  (-) (E a) (E b) = E $ LBinOp (newLabel ()) Sub a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ LLit (newLabel ()) $ LitFloat $ fromInteger a

instance Num (Exp Double) where 
  (+) (E a) (E b) = E $ LBinOp (newLabel ()) Add a b
  (*) (E a) (E b) = E $ LBinOp (newLabel ()) Mul a b
  (-) (E a) (E b) = E $ LBinOp (newLabel ()) Sub a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ LLit (newLabel ()) $ LitDouble $ fromInteger a


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
instance (Num (Exp a), Bits a) => Bits (Exp a) where  
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
  bitSize _ = (bitSize (undefined :: a)) 
  isSigned _ = True 
