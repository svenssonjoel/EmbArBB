{-# LANGUAGE MultiParamTypeClasses,
             ScopedTypeVariables, 
             GADTs,  
             FlexibleInstances #-} 
module Garb where 


import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.Maybe
import Data.Word 
import Data.List as List
import Data.Int

import Text.Show.Functions

import System.IO.Unsafe
import Data.IORef

import qualified Intel.ArbbVM as ArBB
import qualified Intel.ArbbVM.Convenience as ArBB
import qualified Intel.ArbbVM.Type as ArBB

import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr

-- import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as V 

import System.IO.Unsafe

{- 
  TODO: Make it run on ArBB
  TODO: More functionality into the AST
  TODO: Support more types
  TODO: Make it run on CUDA 
  TODO: Make it run on OpenCL 
-} 

-- Question: Why is there no mapM defined on Data.Map ? 

----------------------------------------------------------------------------
-- 76 dashes 

type Label = Word32

{-# NOINLINE counter #-}
counter = unsafePerformIO (newIORef 0) 

newLabel :: () -> Word32
newLabel () = unsafePerformIO $ do 
  p <- readIORef counter
  writeIORef counter (p+1)
  return p 
  
----------------------------------------------------------------------------
--


-- type ScalarType    = ArBB.ScalarType -- Int8 | Int16 | Int32 -- osv
data ContainerType = Scalar ArBB.ScalarType 
                   | Dense Dimensionality ArBB.ScalarType 

data Dimensionality = I | II | III 

----------------------------------------------------------------------------
class V.Storable a => Scalar a where
  typeOf :: a -> ArBB.ScalarType
  sizeOf :: a -> Int 

instance Scalar Int32 where 
  typeOf _ = ArBB.ArbbI32 
  sizeOf a = ArBB.size (typeOf a)

instance Scalar Word32 where 
  typeOf _ = ArBB.ArbbU32
  sizeOf a = ArBB.size  (typeOf a)

  
data Literal = LitInt8   Int8  
             | LitInt32  Int32
             | LitWord32 Word32 
               deriving (Eq,Show)

data Variable = Variable String 
              deriving (Eq, Show)
               
-- Labeled Expression
data LExp = LLit Label Literal  
          | LVar Label Variable 
          | LBinOp Label Op LExp LExp
          | LUnOp Label Op LExp 
            
            -- Index into Vectors 
          | LIndex0 Label LExp 
          | LIndex1 Label LExp LExp -- label vector index
            
            -- Operations on dense  
          | LAddReduce Label LExp 
          | LMulReduce Label LExp 
            
          | LRotate Label LExp LExp 
          | LRotateRev Label LExp LExp
            
          | LSort Label LExp 
          deriving (Show, Eq)
   
getLabel (LSort l _) = l 
getLabel (LRotate l _ _) = l 
getLabel (LRotateRev l _ _) = l
getLabel (LMulReduce l _) = l
getLabel (LAddReduce l _) = l 
getLabel (LIndex1 l _ _) = l 
getLabel (LIndex0 l _) = l 
getLabel (LUnOp l _ _) = l 
getLabel (LBinOp l _ _ _) = l 
getLabel (LVar l _) = l 
getLabel (LLit l _) = l 


                   
data Exp a = E LExp

type ACVector a = Exp (V.Vector a)    

data Op = Add 
        | Sub  
        | Mul 
        | Div 
          deriving (Eq, Show) 
                   
                   
---------------------------------------------------------------------------- 
-- DAG
type NodeID = Word32

data Node = NLit Literal
          | NVar Variable 
          | NBinOp Op NodeID NodeID 
          | NUnOp  Op NodeID 
            
          | NIndex0 NodeID
          | NIndex1 NodeID NodeID 
            
          | NAddReduce NodeID
          | NMulReduce NodeID 
          
          | NRotate NodeID NodeID 
          | NRotateRev NodeID NodeID 
          deriving (Eq,Show)

type DAG = Map.Map NodeID Node
 
------------------------------------------------------------------------------
--        
--substitute :: Exp a -> Id -> Exp b -> Exp c   -- ?      
--substitute (Lit a) i e = Lit a                         
--substitute (Var l j) i e | j < i = Var l j
--                         | j > i = Var l (j-1) 
--                         | j == i = e

--substitute (BinOp l op e1 e2) i e = BinOp l op (substitute e1 i e) (substitute e2 i e)
-- substitute (e1 :*: e2) i e = substitute e1 i e :*: substitute e2 i e
--substitute (Index0 l a) _ _ = Index0 l a
--substitute (Index1 l a e0) i e = Index1 l a (substitute e0 i e) 
--substitute (Index2 l a e0 e1) i e = Index2 l a (substitute e0 i e) (substitute e1 i e)
--substitute (Index3 l a e0 e1 e2) i e = Index3 l a (substitute e0 i e) (substitute e1 i e) (substitute e2 i e) 


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


------------------------------------------------------------------------------
-- Dimensions 
  
{- Support only zero, one, two and threeD arrays, as in ArBB -} 
data Dim = Zero 
         | One Int
         | Two Int Int 
         | Three Int Int Int 
         deriving Show 
                  
-- Quite repetitive, This is one of those places where the accelerate type magic shines.
dim1 a = One (fromIntegral a)                   
dim2 a b = Two (fromIntegral a) (fromIntegral b)  
dim3 a b c = Three (fromIntegral a) (fromIntegral b) (fromIntegral c) 
                  
decrement Zero = error "Cannot go below zero dimensional arrays"          
decrement (One _) = Zero
decrement (Two a _) = One a 
decrement (Three a b _) = Two a b 

dimsToInt Zero = 0
dimsToInt (One _) = 1 
dimsToInt (Two _ _) = 2 
dimsToInt (Three _ _ _) = 3 

sizes (Zero) = [] 
sizes (One a) = [a]
sizes (Two a b) = [a,b] 
sizes (Three a b c) = [a,b,c]

size = foldl (+) 0 .  sizes 
           
type Id = Int -- identification (Index into Map)            
              -- or a De Bruijn index.. 


{- User level arrays. currently just a list of elements and a shape descriptor -}                   
data UserArray = UA {dimensions :: Dim, 
                     contents   :: [Int32]}
               deriving Show 


-- evalOp op a b = evalExp (op (Lit a) (Lit b)) 

--evalExp (Lit l a) = a 
--evalExp (Var l i) = -1 -- error "evalExp: variables not yet implemented" 
--evalExp (BinOp l op e1 e2) = evalBinOp op (evalExp e1) (evalExp e2) 
-- evalExp (e1 :*: e2) = (evalExp e1) * (evalExp e2)
--evalExp (Index0 l a) = error "evalExp: Indexing not yet implemented"
--evalExp _ = error "evalExp: not yet implemented" 

--evalBinOp Add a1 a2 = a1 + a2 
--evalBinOp Mul a1 a2 = a1 * a2
--evalBinOp Sub a1 a2 = a1 - a2 

-- generalMap: correct for 1D,2D,3D. 
--generalMap :: Function -> [[Int32]] -> [Function] 
--generalMap f xs | nub xs ==  [[]] = [] 
--generalMap f xs = applyList f (map (E . Lit . head) xs) : 
--                  generalMap f (map tail xs)  


fold :: (Int32 -> Int32 -> Int32) -> Int32 -> UserArray -> UserArray 
fold op e ua = 
  case dimsToInt (dimensions ua) of 
    0 -> error "cannot fold on zero dimensional array" 
    1 -> UA (decrement (dimensions ua)) 
            [foldr op e (contents ua)]
    2 -> let [a,b] = sizes (dimensions ua) 
         in UA (decrement (dimensions ua))  
               (flatten1 (map (foldr op e) (chop2 a b (contents ua))))
    3 -> let [a,b,c] = sizes (dimensions ua) 
         in UA (decrement (dimensions ua))  
               (flatten2 (map (map (foldr op e)) (chop3 a b c(contents ua))))
      

            
------------------------------------------------------------------------------
chop1 :: Int -> [a] -> [a]         
chop1 _ = id 
chop2 :: Int -> Int -> [a] -> [[a]] 
chop2 _ j as = chop' j as 
chop3 :: Int -> Int -> Int -> [a] -> [[[a]]]
chop3 _ j k as = map (chop' k) (chop' (j*k) as) 
                 
      
chop' n [] = []
chop' n xs = (take n xs) : chop' n (drop n xs) 
            

flatten1 = id                       
flatten2 as = concatMap flatten1 as 
flatten3 as = concatMap flatten2 as



------------------------------------------------------------------------------
-- ArBB Backend things

data ArBBArray = ArBBArray { dim     :: Dim,
                             eltType :: ArBB.ScalarType, 
                             var     :: ArBB.Variable }  
  
-----------------------------------------------------------------------------
-- Generate ArBB function from "Function" 
--genFun :: Function -> ArBB.EmitArbb ArBB.ConvFunction   
--genFun f = do 
--  s <- ArBB.getScalarType_ ArBB.ArbbI32  -- out type (CHEAT) 
--  let its = replicate (arity f) s        -- in  type (CHEAT)
--  fun <- ArBB.funDef_ "generated" [s] its $ \ [out] inp -> do 
--    r <- functionBody (body f) inp 
--    ArBB.copy_ out r
--   -- ArBB.op_ ArBB.ArbbOpMul out inp
--  return fun
         
    
-- Expression to function body, 
-- Any "Arrays" that may occur here will be "global".
--  This will at least be true while "Map" is the only way 
--  to call a function. 
--functionBody :: Exp Word32 -> [ArBB.Variable] -> ArBB.EmitArbb ArBB.Variable
--functionBody (E (Lit l (LitWord32 i))) _ = ArBB.isize_ (fromIntegral i) 
--functionBody (E (Var l (Variable nom))) env = return (env !! nom) 

-- TODO: Bit repetitive! fix. 
--functionBody (E (BinOp l Add e1 e2)) env = do  
--  v1 <- functionBody e1 env 
--  v2 <- functionBody e2 env 
--  s  <- ArBB.getScalarType_ ArBB.ArbbI32
--  r  <- ArBB.createLocal_ s "res"
--  ArBB.op_ ArBB.ArbbOpAdd [r] [v1,v2]
--  return r
--functionBody (BinOp l Mul e1 e2) env = do   
--  v1 <- functionBody e1 env 
--  v2 <- functionBody e2 env 
--  s  <- ArBB.getScalarType_ ArBB.ArbbI32
--  r  <- ArBB.createLocal_ s "res"
--  ArBB.op_ ArBB.ArbbOpMul [r] [v1,v2]
--  return r

-- Index into arrays (This is broken!)
--functionBody (Index0 l arr) _ = undefined
--functionBody (Index1 l arr e1) _ = undefined 
--functionBody (Index2 l arr e1 e2) _ = undefined 
--functionBody (Index3 l arr e1 e2 e3) _ = undefined 

arBBOp Mul = ArBB.ArbbOpMul 
arBBOp Add = ArBB.ArbbOpAdd
arBBOp Sub = ArBB.ArbbOpSub
arBBOp Div = ArBB.ArbbOpDiv
-----------------------------------------------------------------------------
-- return a new (global)  variable   
-- TODO: When doing the non-immediate mode these will be local variables
newArBBArray :: Dim -> ArBB.ScalarType -> ArBB.EmitArbb ArBBArray   
newArBBArray d s = do 
  -- bin <- ArBB.getBindingNull_
  st  <- ArBB.getScalarType_ s -- Type of the elements
  case (dimsToInt d) of 
    
    0 -> do 
      -- return a scalar variable! 
      g   <- ArBB.createGlobal_nobind_ st "tmp" -- bin
      v   <- ArBB.variableFromGlobal_ g
      return$ ArBBArray d s v 
      
    x -> do  
      -- return a dense variable! 
      dt <- ArBB.getDenseType_ st x 
      g  <- ArBB.createGlobal_nobind_ dt "tmp" -- bin
      v  <- ArBB.variableFromGlobal_ g 
      return$ ArBBArray d s v 
  
typeOfArray :: ArBBArray -> ArBB.EmitArbb ArBB.Type
typeOfArray arr = do 
  st <-  ArBB.getScalarType_ (eltType arr)
  case (dimsToInt (dim arr)) of 
    0 -> return st 
    x -> ArBB.getDenseType_ st x 
      
     

------------------------------------------------------------------------------
uploadArrays :: Map.Map Int UserArray -> ArBB.EmitArbb (Map.Map Int ArBBArray) 
uploadArrays  m = do 
  liftIO$ putStrLn "uploading Arrays" 
  l' <- mapM (\(k,v) -> do v' <- uploadArray v
                           return (k,v') ) l 
  return$ Map.fromList l'
  where l = Map.toList m 





uploadArray :: UserArray -> ArBB.EmitArbb ArBBArray 
uploadArray arr@(UA dim d) = do 
  v <- uploadArray' arr
  return (ArBBArray dim ArBB.ArbbI32 v) 
  where 
    uploadArray' :: UserArray -> ArBB.EmitArbb ArBB.Variable 
    uploadArray' (UA dim d) = do               
      -- bin <- ArBB.getBindingNull_
      t   <- ArBB.getScalarType_ ArBB.ArbbI32
      dt  <- ArBB.getDenseType_ t (dimsToInt dim)
      g   <- ArBB.createGlobal_nobind_ dt "in" -- bin
      v   <- ArBB.variableFromGlobal_ g
      s   <- mapM ArBB.usize_ (sizes dim)
      ArBB.opDynamicImm_ ArBB.ArbbOpAlloc [v] s
      ptr <- ArBB.mapToHost_ v [1] ArBB.ArbbWriteOnlyRange

      liftIO$ putStrLn$ "uploading " ++ show (size dim) ++ " elements"
      ArBB.withArray_ d $ \input -> do 
        liftIO$ copyBytes ptr (castPtr input) (size dim * 4)
      -- How do I wait until it is certain that the data is copied ? 
      return v


uploadArrayVector :: V.Vector Int32 -> ArBB.EmitArbb ArBBArray 
uploadArrayVector vector = do -- arr@(UA dim d) = do 
  v <- uploadArray' vector
  return (ArBBArray (One dim) ArBB.ArbbI32 v) 
  where 
    dim = V.length vector 
    uploadArray' :: V.Vector Int32 -> ArBB.EmitArbb ArBB.Variable 
    uploadArray' vector = do               
      -- bin <- ArBB.getBindingNull_
      t   <- ArBB.getScalarType_ ArBB.ArbbI32
      dt  <- ArBB.getDenseType_ t (dimsToInt (One dim))
      g   <- ArBB.createGlobal_nobind_ dt "in" -- bin
      v   <- ArBB.variableFromGlobal_ g
      s   <- mapM ArBB.usize_ (sizes (One dim))
      ArBB.opDynamicImm_ ArBB.ArbbOpAlloc [v] s
      ptr <- ArBB.mapToHost_ v [1] ArBB.ArbbWriteOnlyRange

      liftIO$ putStrLn$ "uploading " ++ show (size (One dim)) ++ " elements"
      liftIO$ V.unsafeWith vector $ \input ->
            copyBytes ptr (castPtr input) (size (One dim) * 4)
      -- How do I wait until it is certain that the data is copied ? 
      return v




------------------------------------------------------------------------------
-- read values back 
readBack :: ArBBArray -> ArBB.EmitArbb UserArray 
readBack arr =  
  let v = var arr
      d = dim arr 
  in case (dimsToInt d) of     
         0 -> do 
           i <- ArBB.readScalar_ v
           return (UA d [i])
         x -> do 
           liftIO$ putStrLn$ "reading back " ++ show (size d) ++ " elements"
           ptr  <- ArBB.mapToHost_ v [1] ArBB.ArbbReadOnlyRange
           dat  <- liftIO$ peekArray (size d) (castPtr ptr)
           return$ UA d dat
  

readBackVector :: ArBBArray -> ArBB.EmitArbb (V.Vector Int32) 
readBackVector arr =  
  let v = var arr
      d = dim arr 
  in case (dimsToInt d) of     
         0 -> error "Only arrays, currently" 
           -- do 
           --i <- ArBB.readScalar_ v
           --return (UA d [i])
         x -> do 
           liftIO$ putStrLn$ "reading back " ++ show (size d) ++ " elements"
           ptr  <- ArBB.mapToHost_ v [1] ArBB.ArbbReadOnlyRange
          
           dat  <- liftIO$ peekArray (size d) (castPtr ptr)
           let vec = V.fromList dat -- Gah ! 
           return vec --UA d dat



                               

------------------------------------------------------------------------------
--   
             
type DAGMaker a = State DAG a  
             
runDAGMaker lexpr = execState lexpr Map.empty
               
compile c = runDAGMaker (compileTest c)              

compileTest :: (ACVector Int32 -> ACVector Int32) -> DAGMaker NodeID
compileTest f = constructDAG res 
  where 
    (E res) = f input
    input = E (LVar (newLabel ()) (Variable "Input"))
    
constructDAG :: LExp -> DAGMaker NodeID 
constructDAG (LVar l v) = 
  do 
    m <- get 
    let m' = Map.insert l (NVar v) m 
    put m'
    return l 
constructDAG (LAddReduce l input) = do     
  m <- get 
  case Map.lookup l m  of 
    (Just nid) -> return (getLabel input) -- (bit messy, use a biMap after all ?) 
    Nothing    -> 
      do 
        input' <- constructDAG input 
        m' <- get 
        let m'' = Map.insert l (NAddReduce input') m'
        put m''
        return l

  
  
    
    
t1 :: ACVector Int32 -> ACVector Int32 
t1 (E input) = E $ LAddReduce (newLabel ()) input 

t2 (E input) = E $ LBinOp (newLabel ()) Add input input

t3 input = t2 (t1 input)
           
{- 
evalAC :: ACVector Int32 -> UserArray 
evalAC (ACInput vector) = let dat = V.toList vector 
                          in UA (One (length dat)) dat
evalAC (ACAddReduce ua) = fold (+) 0 (evalAC ua)
evalAC (ACMulReduce ua) = fold (*) 1 (evalAC ua) 

--evalAC (ACMap f uas) = UA (dimensions (head inputs)) (fixup (generalMap f inputData))                                                                   
--  where inputs = map (\x -> evalAC x) uas
--        inputData = map contents inputs 
evalAC (ACSort a) = UA (dimensions a') (List.sort (contents a'))
  where 
    a' = evalAC a  
    
    
----------------------------------------------------------------------------                                          
-- RunArBB
runArBB :: ACVector Int32 -> V.Vector Int32
runArBB prg = do 
   unsafePerformIO$ ArBB.arbbSession$ 
     do res <- evalArBBImm_AC prg 
        readBackVector res

-- evalArBB_Expr 
        
evalArBBImm_AC :: ACVector Int32 -> ArBB.EmitArbb ArBBArray
evalArBBImm_AC (ACInput vector)  = uploadArrayVector vector 
evalArBBImm_AC (ACRotate vector d) =  do 
  liftIO$ putStrLn "evalArBBImm: Rotate node" 
  arr <- evalArBBImm_AC vector 
  v   <- newArBBArray (dim arr) (eltType arr)
  rt  <- typeOfArray v
  it  <- typeOfArray arr
  fun <- ArBB.funDef_ "rotator" [rt] [it] $ \ out inp -> do 
    d' <- functionBody d []  
    ArBB.opDynamic_ ArBB.ArbbOpRotate out (inp ++ [d'])
    
  ArBB.execute_ fun [var v] [var arr]  
  return v
evalArBBImm_AC (ACRotateRev vector d) =  do 
  liftIO$ putStrLn "evalArBBImm: Rotate node" 
  arr <- evalArBBImm_AC vector 
  v   <- newArBBArray (dim arr) (eltType arr)
  rt  <- typeOfArray v
  it  <- typeOfArray arr
  fun <- ArBB.funDef_ "rotator" [rt] [it] $ \ out inp -> do 
    d' <- functionBody d []  
    ArBB.opDynamic_ ArBB.ArbbOpRotateReverse out (inp ++ [d'])
    
  ArBB.execute_ fun [var v] [var arr]  
  return v
evalArBBImm_AC (ACLiftBinOp op v1 v2) =  do 
  liftIO$ putStrLn "evalArBBImm: Rotate node" 
  a1  <- evalArBBImm_AC v1 
  a2  <- evalArBBImm_AC v2
  v   <- newArBBArray (dim a1) (eltType a1)
  rt  <- typeOfArray v
  it1  <- typeOfArray a1
  it2  <- typeOfArray a2  -- much cheating !
  fun <- ArBB.funDef_ "lifter" [rt] [it1,it2] $ \ out inp -> do 
    ArBB.op_ (arBBOp op) out inp
    
  ArBB.execute_ fun [var v] [var a1,var a2]  
  return v
  


-- do 
--  liftIO$ putStrLn "evalArBBImm: input node!"
--  case (Map.lookup id m) of 
--    Just v -> return v
--    Nothing -> error "evalArBBImm: BUG BUG BUG"
{-    
evalArBBImm_AC (AddReduce a) m = do 
  liftIO$ putStrLn "evalArBBImm: AddReduce node!"
  arr <- evalArBBImm a m
  it    <- typeOfArray arr
  v <- newArBBArray (decrement (dim arr)) (eltType arr)
  rt <- typeOfArray v
  fun <- ArBB.funDef_ "folder" [rt] [it] $ \ out inp -> do 
    ArBB.opDynamic_ ArBB.ArbbOpAddReduce out inp 
    
  ArBB.execute_ fun [var v] [var arr]
  return$ v 
evalArBBImm_AC (Map _ []) _ = error "evalArBBImm: something very wrong"  
evalArBBImm_AC (Map f as) m = do 
  liftIO$ putStrLn "evalArBBImm: Map node"
  arrs <- mapM (\a -> evalArBBImm a m) as
  f' <- genFun f 
  v <- newArBBArray (dim (head arrs)) (eltType (head arrs)) -- wrong!!
  rt <- typeOfArray v
  its <- mapM typeOfArray arrs
       
  mapper <- ArBB.funDef_ "mapper" [rt] its $ \ out inp -> do 
    ArBB.map_ f' out inp 

  ArBB.execute_ mapper [var v] (map var arrs) 
  return v
 -} 
evalArBBImm_AC (ACSort a) = do 
  liftIO$ putStrLn "evalArBBImm: Sort node" 
  arr <- evalArBBImm_AC a 
  v   <- newArBBArray (dim arr) (eltType arr)
  rt  <- typeOfArray v
  it  <- typeOfArray arr
  fun <- ArBB.funDef_ "sorter" [rt] [it] $ \ out inp -> do 
    d <- ArBB.usize_ 0
    ArBB.op_ ArBB.ArbbOpSort out (inp ++ [d])
    
  ArBB.execute_ fun [var v] [var arr]  
  return v

----------------------------------------------------------------------------

sortAC :: V.Vector Int32 -> ACVector Int32
sortAC as =  
  let i = ACInput as
  in ACSort i

testSortAC = evalAC arr 
  where 
    arr = Garb.sortAC (V.fromList [7,6,5,4,3,2,1,0])


testSortArBB = runArBB a
  where 
    a = Garb.sortAC (V.fromList ((reverse [0..4095])++[0..4095]))

-- TODO: Generate ArBB function from something 
--       like this. 
crossProd :: ACVector Int32 -> ACVector Int32 -> ACVector Int32 
crossProd a b = r
  where 
    a' = ACRotate a (Lit 1) 
    b' = ACRotateRev b (Lit 1) 
    lprods = ACLiftBinOp Mul a' b' 
    a'' = ACRotate a' (Lit 1) 
    b'' = ACRotateRev b (Lit 1) 
    rprods = ACLiftBinOp Mul a'' b'' 
    r   = ACLiftBinOp Sub lprods rprods 
    



---------------------------------------------------------------------------- 
-- Testing Testing 
testCrossArBB = runArBB a
  where 
    a = Garb.crossProd (ACInput (V.fromList [1,2,3])) 
                       (ACInput (V.fromList [3,2,1]))


testRotate1 = runArBB a
  where 
    a = ACRotate (ACInput (V.fromList [1,2,3])) (Lit 1) 
 
testRotate2 = runArBB a
  where 
    a = ACRotateRev (ACInput (V.fromList [1,2,3])) (Lit 1) 

testRotate3 = runArBB a
  where 
    a = ACRotate (ACRotate (ACInput (V.fromList [1,2,3])) (Lit 1)) (Lit 1)


testLift = runArBB a
  where 
    a = ACLiftBinOp Add (ACInput (V.fromList [1,1,1])) 
                        (ACInput (V.fromList [2,3,4]))
 



-- TODO: But later it should look like 
{-
crossProd :: AC (Arr Int32) -> AC (Arr Int32) -> AC (Arr Int32) 
crossProd a b = (a <<. 1) *.  (b >>. 1) -.   
                (a <<. 2) *.  (b >>. 2) 
 
-} 

-}