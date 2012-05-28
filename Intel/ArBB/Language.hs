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
import Intel.ArBB.Data
-- TODO: remove this dependency
import Intel.ArBB.Capture

import qualified Intel.ArbbVM as VM

-- Type info needed in cast ops
import Intel.ArBB.Types

import Data.Int
import Data.Word
import Data.Bits


----------------------------------------------------------------------------
-- Create vectors

constVector :: Exp a -> Exp USize -> Exp (DVector Dim1 a) 
constVector (E a) (E s) = 
  E $ Op ConstVector [a,s]


----------------------------------------------------------------------------
-- Specific casts (BIG CHEATS GOING ON HERE) 

toUSize :: Exp (Vector a) -> Exp (Vector USize) 
toUSize (E a) = 
  E $ Op (Cast (Dense I VM.ArbbUsize)) [a]


---------------------------------------------------------------------------- 
-- Reductions 

-- | Reduce along level 0 
addReduce0 :: Num a => Exp (DVector (():.t) a) -> Exp (DVector t a) 
addReduce0 (E vec) = 
  E $ Op AddReduce [vec,zero] 
  where (E zero) = 0 :: Exp USize

-- | Reduce along level 0 
mulReduce0 :: Num a => Exp (DVector (():.t) a) -> Exp (DVector t a) 
mulReduce0 (E vec) = 
  E $ Op MulReduce [vec,zero]
  where (E zero) = 0 :: Exp USize
                                                                             
-- | reduce along a specified level 
addReduce :: Num a => Exp (DVector (():.t) a) -> Exp USize -> Exp (DVector t a) 
addReduce (E vec) (E lev) = 
  E $ Op AddReduce [vec,lev]

-- | reduce along a specified level 
mulReduce :: Num a => Exp (DVector (():.t) a) -> Exp USize -> Exp (DVector t a) 
mulReduce (E vec) (E lev) = 
  E $ Op MulReduce [vec,lev]

-- | reduce along a specified level 
maxReduce :: Num a => Exp (DVector (():.t) a) -> Exp USize -> Exp (DVector t a) 
maxReduce (E vec) (E lev) = 
  E $ Op MaxReduce [vec,lev]

-- | reduce along a specified level 
minReduce :: Num a => Exp (DVector (():.t) a) -> Exp USize -> Exp (DVector t a) 
minReduce (E vec) (E lev) = 
  E $ Op MinReduce [vec,lev]

-- | reduce along a specified level 
andReduce :: Exp (DVector (():.t) Bool) -> Exp USize -> Exp (DVector t Bool) 
andReduce (E vec) (E lev) = 
  E $ Op AndReduce [vec,lev]

-- | reduce along a specified level 
iorReduce :: Exp (DVector (():.t) Bool) -> Exp USize -> Exp (DVector t Bool) 
iorReduce (E vec) (E lev) = 
  E $ Op IorReduce [vec,lev]

-- | reduce along a specified level 
xorReduce :: Exp (DVector (():.t) Bool) -> Exp USize -> Exp (DVector t Bool) 
xorReduce (E vec) (E lev) = 
  E $ Op XorReduce [vec,lev]

---------------------------------------------------------------------------- 
-- Add Merge
-- TODO: what is the type really supposed to be here ?
-- | Add merge..  
addMerge :: Exp (DVector (():.t) a) -> Exp (DVector (():.t) USize) -> Exp USize -> Exp (DVector (():.t) a) 
addMerge (E b) (E v) (E u) = 
  E $ Op AddMerge [b,v,u]

----------------------------------------------------------------------------

-- | zero dimensional vector to scalar
index0 :: Exp (DVector () a) -> Exp a 
index0 (E vec) = E $ Index0  vec 

-- | Index into a 1D vector
index1 :: Exp (DVector Dim1 a) -> Exp USize -> Exp a 
index1 (E vec) (E ix) = 
  E $ Op Extract [vec,ix] 

-- | Index into a 2D vector
index2 :: Exp (DVector Dim2 a) -> Exp USize -> Exp USize -> Exp a 
index2 (E vec) (E ix1) (E ix2)  = 
  E $ Op Extract [vec,ix1,ix2]  

-- | Index into a 3D vector 
index3 :: Exp (DVector Dim2 a) -> Exp USize -> Exp USize -> Exp USize -> Exp a 
index3 (E vec) (E ix1) (E ix2) (E ix3) = 
  E $ Op Extract [vec,ix1,ix2,ix3] 

-- | Extract a row from a 2D vector 
extractRow :: Exp (DVector Dim2 a) -> Exp USize -> Exp (Vector a) 
extractRow (E vec) (E row) = E $ Op ExtractRow [vec,row]

-- | Extract a column from a 2D vector 
extractCol :: Exp (DVector Dim2 a) -> Exp USize -> Exp (Vector a) 
extractCol (E vec) (E col) = E $ Op ExtractCol [vec,col]

-- | Extract a page from a 3D vector 
extractPage :: Exp (DVector Dim3 a) -> Exp USize -> Exp (DVector Dim2 a) 
extractPage (E vec) (E page) = E $ Op ExtractPage [vec,page]

----------------------------------------------------------------------------
-- Repeat, Replace

repeatRow :: Exp (DVector Dim1 a) -> Exp USize -> Exp (DVector Dim2 a) 
repeatRow (E vec) (E u) = E $ Op RepeatRow [vec,u]

replaceCol :: Exp (DVector Dim2 a) -> Exp USize -> Exp (DVector Dim1 a) -> Exp (DVector Dim2 a) 
replaceCol (E m) (E u) (E v) = 
  E $ Op ReplaceCol [m,u,v]

replaceRow :: Exp (DVector Dim2 a) -> Exp USize -> Exp (DVector Dim1 a) -> Exp (DVector Dim2 a) 
replaceRow (E m) (E u) (E v) = 
  E $ Op ReplaceRow [m,u,v]

replacePage :: Exp (DVector Dim3 a) -> Exp USize -> Exp (DVector Dim2 a) -> Exp (DVector Dim3 a) 
replacePage (E m) (E u) (E v) = 
  E $ Op ReplacePage [m,u,v]


---------------------------------------------------------------------------- 
-- Scans 

-- | Scan across a specified level and direction over a dense container
addScan :: Num a 
           => (Exp (DVector t a)) 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector t a)
addScan (E vec) (E dir) (E lev) = 
  E $ Op AddScan [vec,dir,lev] 


-- | Scan across a specified level and direction over a dense container
mulScan :: Num a 
           => (Exp (DVector t a)) 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector t a)
mulScan (E vec) (E dir) (E lev) = 
  E $ Op MulScan [vec,dir,lev] 

-- | Scan across a specified level and direction over a dense container
maxScan :: Num a 
           => (Exp (DVector t a)) 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector t a)
maxScan (E vec) (E dir) (E lev) = 
  E $ Op MaxScan [vec,dir,lev] 
  
-- | Scan across a specified level and direction over a dense container
minScan :: Num a 
           => (Exp (DVector t a)) 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector t a)
minScan (E vec) (E dir) (E lev) = 
  E $ Op MinScan [vec,dir,lev] 
  

-- | Scan across a specified level and direction over a dense container
andScan :: (Exp (DVector t Bool)) 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector t Bool)
andScan (E vec) (E dir) (E lev) = 
  E $ Op AndScan [vec,dir,lev] 
  
-- | Scan across a specified level and direction over a dense container
iorScan ::  (Exp (DVector t Bool)) 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector t Bool)
iorScan (E vec) (E dir) (E lev) = 
  E $ Op IorScan [vec,dir,lev] 
  
-- | Scan across a specified level and direction over a dense container
xorScan ::  (Exp (DVector t Bool)) 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector t Bool)
xorScan (E vec) (E dir) (E lev) = 
  E $ Op XorScan [vec,dir,lev] 

  
  
----------------------------------------------------------------------------
-- | Rotate the contents of a dense container.
-- Example: {1,2,3} -> {2,3,1}
rotate :: Exp (DVector (():.t) a) -> Exp ISize -> Exp (DVector (():.t) a) 
rotate (E vec) (E steps) = 
  E $ Op Rotate [vec,steps]  

-- | Rotate the contents of a dense container in the reversedirection
-- Example: {1,2,3} -> {3,1,2} 
rotateRev :: Exp (DVector (():.t) a) -> Exp ISize -> Exp (DVector (():.t) a) 
rotateRev (E vec) (E steps) = 
  E $ Op RotateRev [vec,steps]  

-- | Reverse a 1D vector 
reverse :: Exp (Vector a) -> Exp (Vector a) 
reverse (E vec) = E $ Op Reverse [vec]

-- | Transpose a the first two dimensionalities of a 2D or 3D container
transpose :: Exp (DVector (():.():.t) a) -> Exp (DVector (():.():. t) a) 
transpose (E vec) = E $ Op Transpose [vec]

-- | Sort the contents of a dense 1D container. Also returns 
-- a dense container of indices describing from where elements where moved
sortRank :: Exp (Vector a) -> Exp USize -> (Exp (Vector a), Exp (Vector USize)) 
sortRank (E vec) (E us)  = (fstPair s, sndPair s)
  where s = E $ Op SortRank [vec,us]

sortRank' :: Exp (Vector a) -> Exp USize -> (Exp (Vector a, Vector USize)) 
sortRank' (E vec) (E us) = 
  E $ Op SortRank [vec,us]

-- | Sort the contents of a dense 1D container. 
sort :: Exp (Vector a) -> Exp USize -> Exp (Vector a) 
sort (E vec) (E us) = 
  E $ Op Sort [vec,us] 

---------------------------------------------------------------------------- 
-- get sizes of vectors 

-- | get the length (total size) of a 1,2,3D vector 
length :: Exp (DVector (():.t) a) -> Exp USize 
length (E vec) = E $ Op Length [vec]

getNRows :: Exp (DVector (():.():.t) a) -> Exp USize 
getNRows (E vec) = E $ Op GetNRows [vec]
  
getNCols :: Exp (DVector (():.():.t) a) -> Exp USize 
getNCols (E vec) = E $ Op GetNCols [vec]

getNPages :: Exp (DVector Dim3 a) -> Exp USize 
getNPages (E vec) = E $ Op GetNPages [vec]

----------------------------------------------------------------------------
-- | Call an ArBB Function 
call :: ArgList t => Function t (Exp r) -> t -> (Exp r) 
call (Function nom) ins = E $ Call nom (argList ins) 

-- TODO: very limited types here. try to generalize
-- TODO: Extremely messy type.. see what can be done 
map :: ArgList (Exp t) => Function (EIn (Exp t) (Exp r)) (EOut (Exp r)) -> Exp (DVector d t) -> Exp (DVector d r) 
map (Function nom) ins = E $ Map nom (argList ins) 

----------------------------------------------------------------------------
-- unpairing. 
fstPair :: Exp (a,b) -> Exp a 
fstPair (E a) = E $ ResIndex a 0
sndPair :: Exp (a,b) -> Exp b
sndPair (E a) = E $ ResIndex a 1 

fst3 :: Exp (a,b,c) -> Exp a
fst3 (E a) = E $ ResIndex a 0
snd3 :: Exp (a,b,c) -> Exp b
snd3 (E a) = E $ ResIndex a 1 
trd3 :: Exp (a,b,c) -> Exp c
trd3 (E a) = E $ ResIndex a 2 

----------------------------------------------------------------------------
-- Stencil (inside map) useful ops 

getNeighbor :: Exp t -> Exp ISize -> Exp ISize -> Exp ISize -> Exp t
getNeighbor (E v) (E p) (E r) (E c) =
    E $ Op GetNeighbor [v,p,r,c]

getNeighbor2D :: Exp t -> Exp ISize -> Exp ISize -> Exp t
getNeighbor2D (E v) (E r) (E c) =
    E $ Op GetNeighbor [v,p,r,c]
  where (E p) = (0 :: Exp ISize) 

eltCoord1D :: () -> Exp USize 
eltCoord1D () = (\(x,y,z) -> z) (getEltCoord ())

eltCoord2D :: () -> (Exp USize, Exp USize) 
eltCoord2D () = (\(p,r,c) -> (r,c)) coord 
    where  
      coord = (getEltCoord ())


eltCoord3D = getEltCoord

getEltCoord :: () -> (Exp USize, Exp USize, Exp USize) 
getEltCoord () = (fst3 e, snd3 e, trd3 e) 
    where e = E $ Op GetEltCoord [] 



----------------------------------------------------------------------------
-- conditional 

ifThenElse :: (Exp Bool) -> (Exp a) -> (Exp a) -> (Exp a)
ifThenElse (E b) (E e1) (E e2) = E $ If b e1 e2


----------------------------------------------------------------------------
--  While Loops 
while :: LoopState state 
          => (state -> Exp Bool)    
          -> (state -> state) 
          -> state 
          -> state 
while cond f state = loopFinalState loop
  where 
    loop = E $ While c body (loopState state)
  --  (curr,vars) = loopVars state
    c = loopCond cond
    body = loopBody f
--    body = loopState (f curr)

-- TODO: Supporting Tuples of Tuples in the loop state would be nice. 
--       Then [LExp] will not cut it anymore.
class LoopState a where 
  loopState :: a -> [Expr]           -- TODO: Again. will structure be needed?
  loopCond :: (a -> Exp Bool) -> [Expr] -> Expr
  loopBody :: (a -> a) -> [Expr] -> [Expr] 
--   loopVars :: a -> (a,[Variable])
  loopFinalState :: Exp s -> a 

  
instance LoopState (Exp a) where   
  loopState (E e1) = [e1]  
  loopCond f [e] = let (E e') = f (E e) in e' 
  loopBody f [e] = let (E e') = f (E e) in [e'] 
--  loopVars  (E e1) = ((e1'),[l1v])
--    where 
--      l1 = newLabel () 
--      l1v = Variable ("l" ++ show l1)
--      e1' = E $ LVar (newLabel ()) l1v 
  loopFinalState (E e) = (E e) -- deconstr then reconstr (to get type right) 

 
instance LoopState (Exp a,Exp b) where   
  loopState (E e1,E e2) = [e1,e2]  
  loopCond f [e1,e2] = let (E e) = f (E e1, E e2) in e
  loopBody f [e1,e2] = let (E e1',E e2') = f (E e1, E e2) in [e1',e2']
  
--  loopVars  (E e1,E e2) = ((e1',e2'),[l1v,l2v])
--    where 
--      l1 = newLabel () 
--      l2 = newLabel () 
--      l1v = Variable ("l" ++ show l1)
--      l2v = Variable ("l" ++ show l2) 
--      e1' = E $ LVar (newLabel ()) l1v 
--      e2' = E $ LVar (newLabel ()) l2v 
  loopFinalState (E e) = (E $ ResIndex e 0, 
                          E $ ResIndex e 1) 
 

instance LoopState (Exp a,Exp b,Exp c) where   
  loopState (e1,e2,e3) = loopState e1 ++ loopState e2 ++ loopState e3  
  loopCond f [e1,e2,e3] = let (E e) = f (E e1, E e2, E e3) in e
  loopBody f [e1,e2,e3] = let (E e1',E e2', E e3') = f (E e1, E e2, E e3) in [e1',e2',e3']

--  loopVars  (e1,e2,e3) = ((e1',e2',e3'),ls1 ++ ls2 ++ ls3 )
--    where 
--      (e1',ls1) = loopVars e1
--      (e2',ls2) = loopVars e2
--      (e3',ls3) = loopVars e3
  
  loopFinalState (E e) = (E $ ResIndex e 0, 
                          E $ ResIndex e 1,
                          E $ ResIndex e 2) 
 
{- 
while :: LoopState state 
          => (state -> Exp Bool)    
          -> (state -> state) 
          -> state 
          -> state 
while cond f state = loopFinalState loop
  where 
    loop = E $ LWhile (newLabel ()) vars c body (loopState state)
    (curr,vars) = loopVars state
    (E c) = cond curr
    body = loopState (f curr)

-- TODO: Supporting Tuples of Tuples in the loop state would be nice. 
--       Then [LExp] will not cut it anymore.
class LoopState a where 
  loopState :: a -> [LExp]           -- TODO: Again. will structure be needed?
  loopVars :: a -> (a,[Variable])
  loopFinalState :: Exp s -> a 

  
instance LoopState (Exp a) where   
  loopState (E e1) = [e1]  
  loopVars  (E e1) = ((e1'),[l1v])
    where 
      l1 = newLabel () 
      l1v = Variable ("l" ++ show l1)
      e1' = E $ LVar (newLabel ()) l1v 
  loopFinalState (E e) = (E e) -- deconstr then reconstr (to get type right) 


instance LoopState (Exp a,Exp b) where   
  loopState (E e1,E e2) = [e1,e2]  
  loopVars  (E e1,E e2) = ((e1',e2'),[l1v,l2v])
    where 
      l1 = newLabel () 
      l2 = newLabel () 
      l1v = Variable ("l" ++ show l1)
      l2v = Variable ("l" ++ show l2) 
      e1' = E $ LVar (newLabel ()) l1v 
      e2' = E $ LVar (newLabel ()) l2v 
  loopFinalState (E e) = (E $ LResIndex (newLabel ()) e 0, 
                          E $ LResIndex (newLabel ()) e 1) 
 

instance LoopState (Exp a,Exp b,Exp c) where   
  loopState (e1,e2,e3) = loopState e1 ++ loopState e2 ++ loopState e3  
  loopVars  (e1,e2,e3) = ((e1',e2',e3'),ls1 ++ ls2 ++ ls3 )
    where 
      (e1',ls1) = loopVars e1
      (e2',ls2) = loopVars e2
      (e3',ls3) = loopVars e3
  
  loopFinalState (E e) = (E $ LResIndex (newLabel ()) e 0, 
                          E $ LResIndex (newLabel ()) e 1,
                          E $ LResIndex (newLabel ()) e 2) 
-} 
  
      


----------------------------------------------------------------------------
-- instances 


instance Show (Exp a) where 
  show (E a) = "exp"

instance Eq (Exp a) where 
  (==) = undefined -- compare labels here

instance Num (Exp Int8) where 
  (+) (E a) (E b) = E $ Op Add [a,b]
  (*) (E a) (E b) = E $ Op Mul [a,b]
  (-) (E a) (E b) = E $ Op Sub [a,b]

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ Lit $ LitInt8 $ fromInteger a

instance Num (Exp Int16) where 
  (+) (E a) (E b) = E $ Op Add [a,b]
  (*) (E a) (E b) = E $ Op Mul [a,b]
  (-) (E a) (E b) = E $ Op Sub [a,b]

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ Lit $ LitInt16 $ fromInteger a

instance Num (Exp Int32) where 
  (+) (E a) (E b) = E $ Op Add [a,b]
  (*) (E a) (E b) = E $ Op Mul [a,b]
  (-) (E a) (E b) = E $ Op Sub [a,b]

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ Lit  $ LitInt32 $ fromInteger a

instance Num (Exp Int64) where 
  (+) (E a) (E b) = E $ Op Add [a,b]
  (*) (E a) (E b) = E $ Op Mul [a,b]
  (-) (E a) (E b) = E $ Op Sub [a,b]

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ Lit $ LitInt64 $ fromInteger a

instance Num (Exp Word8) where 
  (+) (E a) (E b) = E $ Op Add [a,b]
  (*) (E a) (E b) = E $ Op Mul [a,b]
  (-) (E a) (E b) = E $ Op Sub [a,b]

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ Lit $ LitWord8 $ fromInteger a

instance Num (Exp Word16) where 
  (+) (E a) (E b) = E $ Op Add [a,b]
  (*) (E a) (E b) = E $ Op Mul [a,b]
  (-) (E a) (E b) = E $ Op Sub [a,b]

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ Lit $ LitWord16 $ fromInteger a


instance Num (Exp Word32) where 
  (+) (E a) (E b) = E $ Op Add [a,b]
  (*) (E a) (E b) = E $ Op Mul [a,b]
  (-) (E a) (E b) = E $ Op Sub [a,b]

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ Lit  $ LitWord32 $ fromInteger a
  
instance Num (Exp Word64) where 
  (+) (E a) (E b) = E $ Op Add [a,b]
  (*) (E a) (E b) = E $ Op Mul [a,b]
  (-) (E a) (E b) = E $ Op Sub [a,b]

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ Lit $ LitWord64 $ fromInteger a


instance Num (Exp ISize) where 
  (+) (E a) (E b) = E $ Op Add [a,b]
  (*) (E a) (E b) = E $ Op Mul [a,b]
  (-) (E a) (E b) = E $ Op Sub [a,b]

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ Lit $ LitISize $ fromInteger a
  
  
instance Num (Exp USize) where 
  (+) (E a) (E b) = E $ Op Add [a,b]
  (*) (E a) (E b) = E $ Op Mul [a,b]
  (-) (E a) (E b) = E $ Op Sub [a,b]

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ Lit $ LitUSize $ fromInteger a


instance Num (Exp Float) where 
  (+) (E a) (E b) = E $ Op Add [a,b]
  (*) (E a) (E b) = E $ Op Mul [a,b]
  (-) (E a) (E b) = E $ Op Sub [a,b]

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ Lit $ LitFloat $ fromInteger a

instance Num (Exp Double) where 
  (+) (E a) (E b) = E $ Op Add [a,b]
  (*) (E a) (E b) = E $ Op Mul [a,b]
  (-) (E a) (E b) = E $ Op Sub [a,b]

  abs = undefined 
  signum = undefined 
  
  fromInteger a = E $ Lit $ LitDouble $ fromInteger a


----------------------------------------------------------------------------
-- 
instance Num a => Num (Exp (DVector t a)) where 
  (+) (E v1) (E v2) = E $ Op Add [v1,v2] 
  (*) (E v1) (E v2) = E $ Op Mul [v1,v2] 
  (-) (E v1) (E v2) = E $ Op Sub [v1,v2] 
  
  abs = undefined 
  signum = undefined 
  
  fromInteger = undefined 
  
  
----------------------------------------------------------------------------
instance (Num (Exp a), Bits a) => Bits (Exp a) where  
  (.&.) (E a) (E b) = E $ Op Bit_and [a,b]
  (.|.) (E a) (E b) = E $ Op Bit_or [a,b]
  xor (E a) (E b) = E $ Op Bit_xor [a,b]
  
  shiftL (E a) b = E $ Op Lsh [a,dist]
   where 
     (E dist) = fromIntegral b :: (Exp Word8)   
  shiftR (E a) b = E $ Op Rsh [a,dist]
   where 
     (E dist) = fromIntegral b :: (Exp Word8)      
  complement (E a) = E $ Op Bit_not [a]
  bitSize _ = (bitSize (undefined :: a)) 
  isSigned _ = True 


---------------------------------------------------------------------------- 
-- boolean ops 

(<*) :: Ord a => Exp a -> Exp a -> Exp Bool
(<*) (E a) (E b) = E $ Op Less [a,b]



----------------------------------------------------------------------------
-- Floating stuff

instance Fractional (Exp Float) where 
  fromRational = undefined 

instance Floating (Exp Float) where
  pi = E $ Lit (LitFloat pi)
  exp (E a) = E $ Op Exp [a]
  sqrt (E a) = E $ Op  Sqrt [a]
   
  log (E a) = E $ Op Log10 [a] 
  (**) (E a) (E b) = E $ Op Pow [a,b]

  -- log_b(x) = log_e(x) / log_e(b)
  --logBase (Literal 2) b = UnOp Log2 b
  --logBase (Literal 10) b = UnOp Log10 b
  --logBase a b = (UnOp Log b) / (UnOp Log a)
  
  sin (E a) = E $ Op Sin [a]
  tan (E a) = E $ Op Tan [a]
  cos (E a) = E $ Op Cos [a]
   
  asin (E a) = E $ Op Asin [a]
  atan (E a) = E $ Op Atan [a]
  acos (E a) = E $ Op Acos [a]
  
  sinh (E a) = E $ Op Sinh [a]
  tanh (E a) = E $ Op Tanh [a]
  cosh (E a) = E $ Op Cosh [a]
  
  asinh (E a) = undefined -- E $ Op Asinh [a]
  atanh (E a) = undefined -- E $ Op Atanh [a]
  acosh (E a) = undefined -- E $ Op Acosh [a]


