{-# LANGUAGE TypeOperators, 
             FlexibleInstances, 
             FlexibleContexts, 
             UndecidableInstances,
             ScopedTypeVariables #-} 

{- 2012 Joel Svensson -} 
module Intel.ArBB.Language where 

import Intel.ArBB.Vector 
import Intel.ArBB.Syntax 
import Intel.ArBB.Literal
import Intel.ArBB.Variable
import Intel.ArBB.Op
import Intel.ArBB.Data.Int 
import Intel.ArBB.Data.Boolean
import Intel.ArBB.Data

-- import Intel.ArBB.Backend.ArBB
import Intel.ArBB.Reify
import Intel.ArBB.ReifyableType

import qualified Intel.ArbbVM as VM

-- Type info needed in cast ops
import Intel.ArBB.Types

import Prelude as P 

import Data.Int
import Data.Word
import Data.Bits


----------------------------------------------------------------------------
-- Create vectors

constVector :: Exp a -> Exp USize -> Exp (DVector Dim1 a) 
constVector (E a) (E s) = 
  E $ Op ConstVector [a,s]

-- TODO: Really make up mind about a specific order of W and H
constVector2D :: Exp a -> Exp USize -> Exp USize -> Exp (DVector Dim2 a) 
constVector2D a w h = ss'
    where ss = constVector a w 
          ss' = repeatRow ss h 
    

----------------------------------------------------------------------------
-- Specific casts

vecToUSize :: Exp (Vector a) -> Exp (Vector USize) 
vecToUSize (E a) = 
  E $ Op (Cast (Dense I VM.ArbbUsize)) [a]

toUsize :: Exp a -> Exp USize 
toUsize (E a) = 
  E $ Op (Cast (Scalar VM.ArbbUsize)) [a] 

vecToFloat :: Exp (Vector a) -> Exp (Vector Float) 
vecToFloat (E a) = 
  E $ Op (Cast (Dense I VM.ArbbF32)) [a]

vec2DToFloat :: Exp (DVector Dim2 a) -> Exp (DVector Dim2 Float) 
vec2DToFloat (E a) = 
  E $ Op (Cast (Dense II VM.ArbbF32)) [a]

vec3DToFloat :: Exp (DVector Dim3 a) -> Exp (DVector Dim3 Float) 
vec3DToFloat (E a) = 
  E $ Op (Cast (Dense III VM.ArbbF32)) [a]


toFloat :: Exp a -> Exp Float 
toFloat (E a) = 
  E $ Op (Cast (Scalar VM.ArbbF32)) [a] 

vecToDouble :: Exp (Vector a) -> Exp (Vector Double) 
vecToDouble (E a) = 
  E $ Op (Cast (Dense I VM.ArbbF64)) [a]

toDouble :: Exp a -> Exp Double
toDouble (E a) = 
  E $ Op (Cast (Scalar VM.ArbbF64)) [a] 

vecToWord8 :: Exp (Vector a) -> Exp (Vector Word8) 
vecToWord8 (E a) = 
  E $ Op (Cast (Dense I VM.ArbbU8)) [a]

vec2DToWord8 :: Exp (DVector Dim2 a) -> Exp (DVector Dim2 Word8) 
vec2DToWord8 (E a) = 
  E $ Op (Cast (Dense II VM.ArbbU8)) [a]

vec2DToWord32 :: Exp (DVector Dim2 a) -> Exp (DVector Dim2 Word32) 
vec2DToWord32 (E a) = 
  E $ Op (Cast (Dense II VM.ArbbU32)) [a]


toWord8 :: Exp a -> Exp Word8 
toWord8 (E a) = 
  E $ Op (Cast (Scalar VM.ArbbU8)) [a] 

toWord16 :: Exp a -> Exp Word16
toWord16 (E a) = 
  E $ Op (Cast (Scalar VM.ArbbU16)) [a] 

toWord32 :: Exp a -> Exp Word32 
toWord32 (E a) = 
  E $ Op (Cast (Scalar VM.ArbbU32)) [a] 

toWord64 :: Exp a -> Exp Word64 
toWord64 (E a) = 
  E $ Op (Cast (Scalar VM.ArbbU64)) [a] 

convWord8 :: Exp a -> Exp Word8
convWord8 (E a) = 
    E $ Op (BitwiseCast (Scalar VM.ArbbU8)) [a] 

convWord16 :: Exp a -> Exp Word16
convWord16 (E a) = 
    E $ Op (BitwiseCast (Scalar VM.ArbbU16)) [a] 

convWord32 :: Exp a -> Exp Word32
convWord32 (E a) = 
    E $ Op (BitwiseCast (Scalar VM.ArbbU32)) [a] 

convWord64 :: Exp a -> Exp Word64
convWord64 (E a) = 
    E $ Op (BitwiseCast (Scalar VM.ArbbU64)) [a] 



---------------------------------------------------------------------------- 
-- Reductions 

-- | Reduce along level 0 
addReduce0 :: Num a => Exp (DVector (t:.Int) a) -> Exp (DVector t a) 
addReduce0 (E vec) = 
  E $ Op AddReduce [vec,zero] 
  where (E zero) = 0 :: Exp USize

-- | Reduce along level 0 
mulReduce0 :: Num a => Exp (DVector (t:.Int) a) -> Exp (DVector t a) 
mulReduce0 (E vec) = 
  E $ Op MulReduce [vec,zero]
  where (E zero) = 0 :: Exp USize
                                                                             
-- | reduce along a specified level 
addReduce :: Num a => Exp (DVector (t:.Int) a) -> Exp USize -> Exp (DVector t a) 
addReduce (E vec) (E lev) = 
  E $ Op AddReduce [vec,lev]

-- | reduce along a specified level 
mulReduce :: Num a => Exp (DVector (t:.Int) a) -> Exp USize -> Exp (DVector t a) 
mulReduce (E vec) (E lev) = 
  E $ Op MulReduce [vec,lev]

-- | reduce along a specified level 
maxReduce :: Ord a => Exp (DVector (t:.Int) a) -> Exp USize -> Exp (DVector t a) 
maxReduce (E vec) (E lev) = 
  E $ Op MaxReduce [vec,lev]

-- | reduce along a specified level 
minReduce :: Ord a => Exp (DVector (t:.Int) a) -> Exp USize -> Exp (DVector t a) 
minReduce (E vec) (E lev) = 
  E $ Op MinReduce [vec,lev]

-- | reduce along a specified level 
andReduce :: Exp (DVector (t:.Int) Boolean) -> Exp USize -> Exp (DVector t Boolean) 
andReduce (E vec) (E lev) = 
  E $ Op AndReduce [vec,lev]

-- | reduce along a specified level 
iorReduce :: Exp (DVector (t:.Int) Boolean) -> Exp USize -> Exp (DVector t Boolean) 
iorReduce (E vec) (E lev) = 
  E $ Op IorReduce [vec,lev]

-- | reduce along a specified level 
xorReduce :: Exp (DVector (t:.Int) Boolean) -> Exp USize -> Exp (DVector t Boolean) 
xorReduce (E vec) (E lev) = 
  E $ Op XorReduce [vec,lev]

-- | maxReduceLoc computes maximas as well as their locations in the source Vector.
maxReduceLoc :: Ord a => Exp (DVector (t:.Int) a) -> Exp USize -> (Exp (DVector t a), Exp (DVector t USize)) 
maxReduceLoc (E vec) (E lev) = (fstPair res, sndPair res) 
    where 
      res = E $ Op MaxReduceLoc [vec,lev]
-- | minReduceLoc computes minimas as well as their locations in the source Vector.
minReduceLoc :: Ord a => Exp (DVector (t:.Int) a) -> Exp USize -> (Exp (DVector t a), Exp (DVector t USize)) 
minReduceLoc (E vec) (E lev) = (fstPair res, sndPair res) 
    where 
      res = E $ Op MinReduceLoc [vec,lev]

---------------------------------------------------------------------------- 
-- Add Merge
-- | Add merge..  
addMerge :: Exp (DVector (t:.Int) a) -> Exp (DVector (t:.Int) USize) -> Exp USize -> Exp (DVector (t:.Int) a) 
addMerge (E b) (E v) (E u) = 
  E $ Op AddMerge [b,v,u]

----------------------------------------------------------------------------

-- | zero dimensional vector to scalar
index0 :: Exp (DVector Z a) -> Exp a 
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

replace1D :: Exp (DVector Dim1 a) 
          -> Exp USize 
          -> Exp USize 
          -> Exp USize 
          -> Exp (DVector Dim1 a) 
          -> Exp (DVector Dim1 a) 
replace1D (E dst) (E first) (E n) (E stride) (E src) =
    E $ Op Replace [dst,first,n,stride,src] 

fill :: Exp (DVector Dim1 a) 
     -> Exp a -- fill value 
     -> Exp USize -- start 
     -> Exp USize -- end 
     -> Exp (DVector Dim1 a) 
fill dst val start end = 
    replace1D dst start n 1 cv
    where 
      n  = end - start + 1 
      cv = constVector val n 
        

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
andScan :: (Exp (DVector t Boolean)) 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector t Boolean)
andScan (E vec) (E dir) (E lev) = 
  E $ Op AndScan [vec,dir,lev] 
  
-- | Scan across a specified level and direction over a dense container
iorScan ::  (Exp (DVector t Boolean)) 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector t Boolean)
iorScan (E vec) (E dir) (E lev) = 
  E $ Op IorScan [vec,dir,lev] 
  
-- | Scan across a specified level and direction over a dense container
xorScan ::  (Exp (DVector t Boolean)) 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector t Boolean)
xorScan (E vec) (E dir) (E lev) = 
  E $ Op XorScan [vec,dir,lev] 

  
  
----------------------------------------------------------------------------
-- | Rotate the contents of a dense container.
-- Example: {1,2,3} -> {2,3,1}
rotate :: Exp (DVector (Int:.t) a) -> Exp ISize -> Exp (DVector (Int:.t) a) 
rotate (E vec) (E steps) = 
  E $ Op Rotate [vec,steps]  

-- | Rotate the contents of a dense container in the reversedirection
-- Example: {1,2,3} -> {3,1,2} 
rotateRev :: Exp (DVector (t:.Int) a) -> Exp ISize -> Exp (DVector (t:.Int) a) 
rotateRev (E vec) (E steps) = 
  E $ Op RotateRev [vec,steps]  

-- | Reverse a 1D vector 
reverse :: Exp (Vector a) -> Exp (Vector a) 
reverse (E vec) = E $ Op Reverse [vec]

-- | Transpose a the first two dimensionalities of a 2D or 3D container
transpose :: Exp (DVector (t:.Int:.Int) a) -> Exp (DVector (t:.Int:.Int) a) 
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

-- | interleave elements from different dense containers
shuffle :: Exp (DVector t a) -> Exp (DVector t a) -> Exp USize -> Exp (DVector t a) 
shuffle (E v1) (E v2) (E u) = E $ Op Shuffle [v1,v2,u,zero]
    where (E zero) = 0 :: Exp USize 

-- Shows an inconsistency. Here both Nested and Dense require the same number of arguemnts 
-- So the the zero is just a dummy (not used in the dense case) 

-- | the inverse of shuffle 
unshuffle :: Exp (DVector t a) -> Exp USize -> Exp (DVector t a) 
unshuffle (E v) (E u) = E $ Op Unshuffle [v,u,zero]
    where (E zero) = 0 :: Exp USize 

--Another inconsistency. Other ArBB ops have two outputs. This one concatenates its
-- two output vectors..

-- | Gather 
gather1D :: Exp (DVector Dim1 a) -> Exp (DVector Dim1 USize) -> Exp a -> Exp (DVector Dim1 a) 
gather1D (E v) (E cols) (E a) = E $ Op Gather [v,cols,a] 

gather2D :: Exp (DVector Dim2 a) 
          -> Exp (DVector Dim2 USize) 
          -> Exp (DVector Dim2 USize) 
          -> Exp a 
          -> Exp (DVector Dim2 a) 
gather2D (E v) (E rows) (E cols) (E a) = E $ Op Gather [v,rows,cols,a] 


gather3D :: Exp (DVector Dim3 a) 
          -> Exp (DVector Dim3 USize) 
          -> Exp (DVector Dim3 USize) 
          -> Exp (DVector Dim3 USize) 
          -> Exp a 
          -> Exp (DVector Dim3 a) 
gather3D (E v) (E pages) (E rows) (E cols) (E a) = E $ Op Gather [v,pages,rows,cols,a] 

-- | Scatter
scatter1D :: Exp (DVector Dim1 a) 
           -> Exp (DVector Dim1 USize) 
           -> Exp (DVector Dim1 a) 
           -> Exp (DVector Dim1 a) 
scatter1D (E inp) (E cols) (E into) = E $ Op Scatter [inp,cols,into] 

scatter2D :: Exp (DVector Dim2 a) 
           -> Exp (DVector Dim2 USize) 
           -> Exp (DVector Dim2 USize) 
           -> Exp (DVector Dim2 a) 
           -> Exp (DVector Dim2 a) 
scatter2D (E inp) (E rows) (E cols) (E into) = E $ Op Scatter [inp,rows,cols,into] 

scatter3D :: Exp (DVector Dim3 a) 
           -> Exp (DVector Dim3 USize) 
           -> Exp (DVector Dim3 USize) 
           -> Exp (DVector Dim3 USize) 
           -> Exp (DVector Dim3 a) 
           -> Exp (DVector Dim3 a) 
scatter3D (E inp) (E pages) (E rows) (E cols) (E into) 
    = E $ Op Scatter [inp,pages,rows,cols,into] 

-- | compacts data in a container using a mask container. 
--   Works for nested and dense containers.
pack :: (IsVector v a, IsVector v Boolean) 
      => Exp (v a) -> Exp (v Boolean) -> Exp (v a) 
pack (E v) (E b) = E $ Op Pack [v,b]

-- | unpack a vector. 
unpack :: (IsVector v a, IsVector v Boolean) 
        => Exp (v a) -> Exp (v Boolean) -> Exp a -> Exp (v a) 
unpack (E v) (E b) (E a) = E $ Op Unpack [v,b,a]

-- | repeat elements of a container
distribute :: Exp (DVector (t:.Int) a) -> Exp USize -> Exp (DVector (t:.Int) a) 
distribute (E v) (E u) = E $ Op Distribute [v,u,zero] 
    where (E zero) = 0 :: Exp USize         

-- TODO: rename this once you figure out what it does. 
distribute2 :: Exp (DVector (t:.Int) a) 
             -> Exp (DVector (t:.Int) USize) 
             -> Exp (DVector (t:.Int) a) 
distribute2 (E v) (E us) = E $ Op Distribute [v,us,zero] 
    where (E zero) = 0 :: Exp USize                           
             

swapRow :: Exp (DVector (t:.Int:.Int) a) 
         -> Exp USize 
         -> Exp USize 
         -> Exp (DVector (t:.Int:.Int) a)
swapRow (E v) (E i) (E j) = E $ Op SwapRow [v,i,j]

swapCol :: Exp (DVector (t:.Int:.Int) a) 
         -> Exp USize 
         -> Exp USize 
         -> Exp (DVector (t:.Int:.Int) a) 
swapCol (E v) (E i) (E j) = E $ Op SwapCol [v,i,j]

swapPage :: Exp (DVector Dim3 a) 
          -> Exp USize 
          -> Exp USize 
          -> Exp (DVector Dim3 a) 
swapPage (E v) (E i) (E j) = E $ Op SwapPage [v,i,j]

shift1D :: Exp (DVector Dim1 a) -> Exp ISize -> Exp (DVector Dim1 a) 
shift1D (E v) (E i) = E $ Op ShiftConst [v,i]

shift2D :: Exp (DVector Dim2 a) -> Exp ISize -> Exp ISize -> Exp (DVector Dim2 a) 
shift2D (E v) (E i) (E j)  = E $ Op ShiftConst [v,i,j]

shift3D :: Exp (DVector Dim3 a) -> Exp ISize -> Exp ISize -> Exp ISize -> Exp (DVector Dim3 a) 
shift3D (E v) (E i) (E j) (E k)  = E $ Op ShiftConst [v,i,j,k]

shiftRev1D :: Exp (DVector Dim1 a) -> Exp ISize -> Exp (DVector Dim1 a) 
shiftRev1D (E v) (E i) = E $ Op ShiftConstRev [v,i]

shiftRev2D :: Exp (DVector Dim2 a) -> Exp ISize -> Exp ISize -> Exp (DVector Dim2 a) 
shiftRev2D (E v) (E i) (E j)  = E $ Op ShiftConstRev [v,i,j]

shiftRev3D :: Exp (DVector Dim3 a) -> Exp ISize -> Exp ISize -> Exp ISize -> Exp (DVector Dim3 a) 
shiftRev3D (E v) (E i) (E j) (E k)  = E $ Op ShiftConstRev [v,i,j,k]

shiftClamp1D :: Exp (DVector Dim1 a) -> Exp ISize -> Exp (DVector Dim1 a) 
shiftClamp1D (E v) (E i) = E $ Op ShiftClamp [v,i]

shiftClamp2D :: Exp (DVector Dim2 a) -> Exp ISize -> Exp ISize -> Exp (DVector Dim2 a) 
shiftClamp2D (E v) (E i) (E j)  = E $ Op ShiftClamp [v,i,j]

shiftClamp3D :: Exp (DVector Dim3 a) -> Exp ISize -> Exp ISize -> Exp ISize -> Exp (DVector Dim3 a) 
shiftClamp3D (E v) (E i) (E j) (E k)  = E $ Op ShiftClamp [v,i,j,k]

shiftClampRev1D :: Exp (DVector Dim1 a) -> Exp ISize -> Exp (DVector Dim1 a) 
shiftClampRev1D (E v) (E i) = E $ Op ShiftClampRev [v,i]

shiftClampRev2D :: Exp (DVector Dim2 a) -> Exp ISize -> Exp ISize -> Exp (DVector Dim2 a) 
shiftClampRev2D (E v) (E i) (E j)  = E $ Op ShiftClampRev [v,i,j]

shiftClampRev3D :: Exp (DVector Dim3 a) -> Exp ISize -> Exp ISize -> Exp ISize -> Exp (DVector Dim3 a) 
shiftClampRev3D (E v) (E i) (E j) (E k)  = E $ Op ShiftClampRev [v,i,j,k]

cat :: IsVector t a => Exp (t a) -> Exp (t a) -> Exp (t a) 
cat (E v1) (E v2) = E $ Op Cat [v1,v2] 

---------------------------------------------------------------------------- 
-- get sizes of vectors 

-- | get the length (total size) of a 1,2,3D vector 
length :: Exp (DVector (t:.Int) a) -> Exp USize 
length (E vec) = E $ Op Length [vec]

getNRows :: Exp (DVector (t:.Int:.Int) a) -> Exp USize 
getNRows (E vec) = E $ Op GetNRows [vec]
  
getNCols :: Exp (DVector (t:.Int:.Int) a) -> Exp USize 
getNCols (E vec) = E $ Op GetNCols [vec]

getNPages :: Exp (DVector Dim3 a) -> Exp USize 
getNPages (E vec) = E $ Op GetNPages [vec]


----------------------------------------------------------------------------
-- Section. 

section1D :: Exp (DVector Dim1 a) 
           -> Exp USize 
           -> Exp USize 
           -> Exp USize 
           -> Exp (DVector Dim1 a) 
section1D (E v) (E offs) (E len) (E stride) = E $ Op Section [v,offs,len,stride] 

section2D :: Exp (DVector Dim2 a) 
          -> Exp USize -> Exp USize -> Exp USize 
          -> Exp USize -> Exp USize -> Exp USize 
          -> Exp (DVector Dim2 a)
section2D (E v) (E ro) (E nr) (E rs) (E co) (E nc) (E cs) = 
    E $ Op Section [v,ro,nr,rs,co,nc,cs]


section3D :: Exp (DVector Dim3 a) 
          -> Exp USize -> Exp USize -> Exp USize 
          -> Exp USize -> Exp USize -> Exp USize 
          -> Exp USize -> Exp USize -> Exp USize 
          -> Exp (DVector Dim3 a)
section3D (E v) (E pr) (E np) (E ps) (E ro) (E nr) (E rs) (E co) (E nc) (E cs) = 
    E $ Op Section [v,pr,np,ps,ro,nr,rs,co,nc,cs]

----------------------------------------------------------------------------
-- NESTED OPS 

--split :: Exp (DVector t a) -> Exp (DVector t ISize) -> Exp (NVector a) 
-- | split a Dense or Nested vector given a vector of integers (-1,0 or 1) 
split :: (IsVector t e) => Exp (t e) -> Exp (t ISize) -> Exp (NVector a)
split (E a) (E i) = E $ Op Split [a,i] 

-- | unsplit 
unsplit :: (IsVector t ISize) => Exp (NVector e) -> Exp (t ISize) -> Exp (NVector a) 
unsplit (E n) (E i) = E $ Op Unsplit [n,i]

-- | segment extracts a segment from a nested vector. 
segment :: Exp (NVector a) -> Exp USize -> Exp (DVector Dim1 a) 
segment (E a) (E u) = E $ Op Segment [a,u]

-- would want to require that t is either nested, or of dim2  or higher. 
flatten :: (IsVector t e) => Exp (t e) -> Exp (DVector Dim1 a) 
flatten (E a) = E $ Op Flatten [a] 

flattenDense :: Exp (DVector (t:.Int:.Int) a) -> Exp (DVector Dim1 a) 
flattenDense (E a) = E $ Op Flatten [a]  

flattenSeg :: Exp (NVector a) -> Exp (DVector Dim1 a) 
flattenSeg (E a) = E $ Op Flatten [a]  

replaceSeg :: Exp (NVector a) -> Exp USize -> Exp (DVector Dim1 a) -> Exp (NVector a) 
replaceSeg (E n) (E u) (E d) = E $ Op ReplaceSegment [n,u,d]

indexSeg :: Exp (NVector a) -> Exp USize -> Exp USize -> Exp a 
indexSeg (E n) (E s) (E i) = E $ Op Extract [n,s,i] 

-- | interleave segments from different nested containers
shuffleSegments :: Exp (NVector a) -> Exp (NVector a) -> Exp USize -> Exp (NVector a) 
shuffleSegments (E n1) (E n2) (E u) = E $ Op Shuffle [n1,n2,u,one]
    where (E one) = 1 :: Exp USize

-- | interleave elements from different nested containers
shuffleSeg :: Exp (NVector a) -> Exp (NVector a) -> Exp USize -> Exp (NVector a) 
shuffleSeg (E n1) (E n2) (E u) = E $ Op Shuffle [n1,n2,u,zero] 
    where (E zero) = 0 :: Exp USize 

-- | unshuffles on a per segment basis 
unshuffleSegments :: Exp (NVector a) -> Exp USize -> Exp (NVector a) 
unshuffleSegments (E n) (E u) = E $ Op Unshuffle [n,u,one] 
    where (E one) = 1 :: Exp USize

-- | ushuffles on a per element basis
unshuffleSeg :: Exp (NVector a) -> Exp USize -> Exp (NVector a) 
unshuffleSeg (E n) (E u) = E $ Op Unshuffle [n,u,zero]
    where (E zero) = 0 :: Exp USize 

-- TODO: Something may be broken when it comes to setRegularNesting. 
--       Figure out what and where.. 
-- | apply regular nesting to a container 
setRegularNesting2D :: Exp (DVector Dim1 a) -> Exp USize -> Exp USize -> Exp (DVector Dim2 a)
setRegularNesting2D (E v) (E h) (E w) = E $ Op SetRegularNesting [v,h,w]

setRegularNesting3D :: Exp (DVector Dim1 a) -> Exp USize -> Exp USize -> Exp USize -> Exp (DVector Dim3 a) 
setRegularNesting3D (E v) (E h) (E w) (E p) = E $ Op SetRegularNesting [v,h,w,p] 

-- Will only support the USize nesting descriptors for now. 
applyNesting :: Exp (DVector Dim1 a) -> Exp (DVector Dim1 USize) -> Exp USize -> Exp (NVector a)
applyNesting (E v) (E u) (E nt) = E $ Op ApplyNesting [v,u,nt] 

getNesting :: Exp (NVector a) -> Exp USize -> Exp (DVector Dim1 USize) 
getNesting (E v) (E n) = E $ Op GetNesting [v,n]

copyNesting :: Exp (DVector Dim1 a) -> Exp (NVector b) -> Exp (NVector a) 
copyNesting (E v) (E n) = E $ Op CopyNesting [v,n]

replaceSegment :: Exp (NVector a) -> Exp USize -> Exp (DVector Dim1 a) -> Exp (NVector a) 
replaceSegment (E n) (E u) (E d) = E $ Op ReplaceSegment [n,u,d]

addReduceSeg :: Num a => Exp (NVector a) -> Exp (DVector Dim1 a)
addReduceSeg (E v) = E $ Op AddReduce [v,zero]
    where (E zero) = 0 :: Exp USize

mulReduceSeg:: Num a => Exp (NVector a) -> Exp (DVector Dim1 a)
mulReduceSeg (E v) = E $ Op MulReduce [v,zero]
    where (E zero) = 0 :: Exp USize

maxReduceSeg:: Num a => Exp (NVector a) -> Exp (DVector Dim1 a)
maxReduceSeg (E v) = E $ Op MaxReduce [v,zero]
    where (E zero) = 0 :: Exp USize

minReduceSeg:: Num a => Exp (NVector a) -> Exp (DVector Dim1 a)
minReduceSeg (E v) = E $ Op MinReduce [v,zero]
    where (E zero) = 0 :: Exp USize

andReduceSeg :: Exp (NVector Boolean) -> Exp (DVector Dim1 Boolean)
andReduceSeg (E v) = E $ Op AndReduce [v,zero]
    where (E zero) = 0 :: Exp USize

iorReduceSeg :: Exp (NVector Boolean) -> Exp (DVector Dim1 Boolean)
iorReduceSeg (E v) = E $ Op IorReduce [v,zero]
    where (E zero) = 0 :: Exp USize

xorReduceSeg :: Exp (NVector Boolean) -> Exp (DVector Dim1 Boolean)
xorReduceSeg (E v) = E $ Op XorReduce [v,zero]
    where (E zero) = 0 :: Exp USize

addScanSeg :: Num a => Exp (NVector a) -> Exp (NVector a) 
addScanSeg (E v) = E $ Op AddScan [v,zero,zero]
    where (E zero) = 0 :: Exp USize

mulScanSeg :: Num a => Exp (NVector a) -> Exp (NVector a) 
mulScanSeg (E v) = E $ Op MulScan [v,zero,zero]
    where (E zero) = 0 :: Exp USize

maxScanSeg :: Num a => Exp (NVector a) -> Exp (NVector a) 
maxScanSeg (E v) = E $ Op MaxScan [v,zero,zero]
    where (E zero) = 0 :: Exp USize

minScanSeg :: Num a => Exp (NVector a) -> Exp (NVector a) 
minScanSeg (E v) = E $ Op MinScan [v,zero,zero]
    where (E zero) = 0 :: Exp USize

andScanSeg :: Exp (NVector Boolean) -> Exp (NVector Boolean) 
andScanSeg (E v) = E $ Op AndScan [v,zero,zero]
    where (E zero) = 0 :: Exp USize

iorScanSeg :: Exp (NVector Boolean) -> Exp (NVector Boolean) 
iorScanSeg (E v) = E $ Op IorScan [v,zero,zero]
    where (E zero) = 0 :: Exp USize

xorScanSeg :: Exp (NVector Boolean) -> Exp (NVector Boolean) 
xorScanSeg (E v) = E $ Op XorScan [v,zero,zero]
    where (E zero) = 0 :: Exp USize
 

distributeSeg :: Exp (NVector a) -> Exp USize -> Exp (NVector a) 
distributeSeg (E v) (E u) = E $ Op Distribute [v,u,zero] 
    where (E zero) = 0 :: Exp USize 

distributeSegment :: Exp (NVector a) -> Exp USize -> Exp (NVector a) 
distributeSegment (E v) (E u) = E $ Op Distribute [v,u,one]
    where (E one) = 1 :: Exp USize 


transposeSeg :: Exp (NVector a) -> Exp (NVector a) 
transposeSeg (E v) = E $ Op Transpose [v]


----------------------------------------------------------------------------
-- | Map an ArBB Function 
map :: (Data a, Data b) => (Exp a -> Exp b) -> Exp (DVector t a) -> Exp (DVector t b)
map f (E v) = E $ Map (reify f) [v] 

-- | Call an ArBB Function 
call :: (Data a, Data b) => (Exp a -> Exp b) -> Exp a -> Exp b 
call f (E a) = E $ Call (reify f) [a] 

-- | zipWith
zipWith :: (Data a, Data b, Data c) 
         => (Exp a -> Exp b -> Exp c) 
         -> Exp (DVector t a) 
         -> Exp (DVector t b) 
         -> Exp (DVector t c) 
zipWith f (E v1) (E v2) = E $ Map (reify f) [v1,v2] 

zipWith3 :: (Data a, Data b, Data c, Data d) 
         => (Exp a -> Exp b -> Exp c -> Exp d) 
         -> Exp (DVector t a) 
         -> Exp (DVector t b) 
         -> Exp (DVector t c) 
         -> Exp (DVector t d) 
zipWith3 f (E v1) (E v2) (E v3) = E $ Map (reify f) [v1,v2,v3] 

zipWith4 :: (Data a, Data b, Data c, Data d, Data e) 
         => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e) 
         -> Exp (DVector t a) 
         -> Exp (DVector t b) 
         -> Exp (DVector t c) 
         -> Exp (DVector t d) 
         -> Exp (DVector t e) 
zipWith4 f (E v1) (E v2) (E v3) (E v4) = E $ Map (reify f) [v1,v2,v3,v4] 

zipWith5 :: (Data a, Data b, Data c, Data d, Data e, Data f) 
         => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f) 
         -> Exp (DVector t a) 
         -> Exp (DVector t b) 
         -> Exp (DVector t c) 
         -> Exp (DVector t d) 
         -> Exp (DVector t e) 
         -> Exp (DVector t f) 
zipWith5 f (E v1) (E v2) (E v3) (E v4) (E v5) 
    = E $ Map (reify f) [v1,v2,v3,v4,v5] 

zipWith6 :: (Data a, Data b, Data c, Data d, Data e, Data f, Data g) 
         => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g) 
         -> Exp (DVector t a) 
         -> Exp (DVector t b) 
         -> Exp (DVector t c) 
         -> Exp (DVector t d) 
         -> Exp (DVector t e) 
         -> Exp (DVector t f) 
         -> Exp (DVector t g) 
zipWith6 f (E v1) (E v2) (E v3) (E v4) (E v5) (E v6) 
    = E $ Map (reify f) [v1,v2,v3,v4,v5,v6] 

zipWith7 :: (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h) 
         => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h) 
         -> Exp (DVector t a) 
         -> Exp (DVector t b) 
         -> Exp (DVector t c) 
         -> Exp (DVector t d) 
         -> Exp (DVector t e) 
         -> Exp (DVector t f) 
         -> Exp (DVector t g) 
         -> Exp (DVector t h) 
zipWith7 f (E v1) (E v2) (E v3) (E v4) (E v5) (E v6) (E v7) 
    = E $ Map (reify f) [v1,v2,v3,v4,v5,v6,v7] 



----------------------------------------------------------------------------
-- Stencil operations. 

data Stencil a d =  Stencil [a] d 

---------------------------------------------------------------------------- 
-- Experimental. 
mapStencil :: (Num (Exp a), Data a, Dimensions t ) 
            => Stencil (Exp a) t 
             -> Exp (DVector t a) 
             -> Exp (DVector t a)
mapStencil (Stencil ls d)  (E v) = E $ Map (reify f) [v] 
   where 
     -- create the function to be mapped. 
     dim = toDim d 
     
     coords = coordsFromDim dim 
     cs     = zip ls coords 
     (coeff, pos) = unzip cs 
     f e = foldl (+) 0 $ P.zipWith (*) 
                       [getNeighbor e 
                                    (fromIntegral p) 
                                    (fromIntegral c) 
                                    (fromIntegral r)  | (p,c,r) <- pos] 
                                      coeff
                    
coordsFromDim (Dim xs) = coordsFromDim' xs
    where
      coordsFromDim' [] = []
      -- coordsFromDim' (r:[]) = [(0,0,i) | i <- [0..r-1]]       
      coordsFromDim' (r:c:[]) = concat [[(0,j,i) | i <- [(-((r-1)`div`2))..((r-1)`div`2)]]| j <- [(-((c-1)`div`2))..((c-1)`div`2)]]
      -- coordsFromDim' (r:c:p:[]) = concat $ concat [[[(k,j,i) | i <- [0..r-1]]| j <- [0..c-1]]| k <- [0..p-1]]

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

ifThenElse :: (Exp Boolean) -> (Exp a) -> (Exp a) -> (Exp a)
ifThenElse (E b) (E e1) (E e2) = E $ If b e1 e2


----------------------------------------------------------------------------
--  While Loops 
while :: LoopState state 
          => (state -> Exp Boolean)    
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
  loopCond :: (a -> Exp Boolean) -> [Expr] -> Expr
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
  
  fromInteger a = E $ Lit $ LitFloat $ (fromInteger a :: Float)

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
  
instance Fractional a => Fractional (Exp (DVector t a)) where 
    (/) (E v1) (E v2) = E $ Op Div [v1,v2]
    recip = undefined
    fromRational = undefined 


instance Num a => Ord  (Exp (DVector t a)) -- Cheat
instance Enum (Exp (DVector t a)) -- Cheat
instance Num a => Real (Exp (DVector t a)) -- Cheat 
    

-- TODO: implement
instance Integral a => Integral (Exp (DVector t a)) where 
    div (E v1) (E v2) = E $ Op Div [v1,v2] 
    quot = undefined 
    rem = undefined 
    mod = undefined 
    quotRem = undefined 
    divMod = undefined 
    toInteger = undefined 



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

(<*) :: Ord a => Exp a -> Exp a -> Exp Boolean
(<*) (E a) (E b) = E $ Op Less [a,b]



----------------------------------------------------------------------------
-- Floating stuff

instance Fractional (Exp Float) where 
  (/) (E a) (E b) = E $ Op Div [a,b]
  fromRational = error "fromRational: APA"

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


instance Ord (Exp Float) where 
  min (E a) (E b) = E $ Op Min [a,b]
  max (E a) (E b) = E $ Op Max [a,b]

instance Ord (Exp Word32) where 
  min (E a) (E b) = E $ Op Min [a,b]
  max (E a) (E b) = E $ Op Max [a,b]

instance Ord (Exp USize) where 
  min (E a) (E b) = E $ Op Min [a,b]
  max (E a) (E b) = E $ Op Max [a,b]