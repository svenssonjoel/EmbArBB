{-# LANGUAGE MultiParamTypeClasses,
             ScopedTypeVariables, 
             GADTs #-} 
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


--data Value = IntVal Int 
--           | FloatVal Float 
--           | DoubleVal Double
--             deriving (Eq,Show) 
             
-- TODO: Typed exps or the Value approach above
data Exp = Lit Int32 --- Value     -- One of the supported value types
         | Var Label Id            -- De Bruijn index (ok? I am on thin ice here) 
         | BinOp Label Op Exp Exp  
         | UnOp  Label Op Exp  
           
         -- Index into 0D, 1D, 2D, 3D Array   
         -- May need another rep of arrays here.
         -- In the ArBB backend and array here will just be a "ArBB.Variable" 
         -- These should not be "arrays" they should be indices into some Map 
         -- that holds ArBB Variables in the ArBB case.
     --    | Index0 Label Array
     --    | Index1 Label Array Exp
     --    | Index2 Label Array Exp Exp 
     --    | Index3 Label Array Exp Exp Exp 
           deriving Show
                    
data Op = Add 
        | Sub  
        | Mul 
        | Div 
          deriving (Eq, Show) 
 

------------------------------------------------------------------------------
-- Represent functions (to be mapped etc) 
-- As it is now this allows no nested parallelism. 
data Function = Lam Function                     
              | E Exp 
                deriving Show                  
                                                  
arity (E a) = 0
arity (Lam x) = 1 + arity x

body (E a) = a 
body (Lam f) = body f

apply :: Function -> Function -> Function
apply (Lam x) (E y) = apply' x y 0                    
  where apply' (E x) y i    = E (substitute x i y)
        apply' (Lam  x) y i = Lam (apply' x y (i+1))
apply f x = error ("apply: " ++ show f) 
 
applyList :: Function -> [Function] -> Function
applyList (E x) [] = E x                                                                                         
applyList (Lam f) (x:xs) = applyList (apply (Lam f) x) xs
applyList f x = error ("applyList: " ++ show x ++ " " ++ show f )

------------------------------------------------------------------------------
--        
substitute :: Exp -> Id -> Exp -> Exp         
substitute (Lit a) i e = Lit a                         
substitute (Var l j) i e | j < i = Var l j
                         | j > i = Var l (j-1) 
                         | j == i = e

substitute (BinOp l op e1 e2) i e = BinOp l op (substitute e1 i e) (substitute e2 i e)
-- substitute (e1 :*: e2) i e = substitute e1 i e :*: substitute e2 i e
--substitute (Index0 l a) _ _ = Index0 l a
--substitute (Index1 l a e0) i e = Index1 l a (substitute e0 i e) 
--substitute (Index2 l a e0 e1) i e = Index2 l a (substitute e0 i e) (substitute e1 i e)
--substitute (Index3 l a e0 e1 e2) i e = Index3 l a (substitute e0 i e) (substitute e1 i e) (substitute e2 i e) 

{-                         
add = Lam (Lam (E (BinOp Add (Var 0) (Var 1)))) 

mul = Lam (Lam (E (BinOp Mul (Var 0) (Var 1))))

square = Lam (E (BinOp Mul (Var 0) (Var 0)))
    
comp f g = Lam (apply f (apply g (E (Var 0))))

addone = apply add (E (Lit 1))
multwo = apply mul (E (Lit 2))
-} 
------------------------------------------------------------------------------ 
-- Experiment
instance Eq Function where 
  (==) = undefined 

instance Num Function where 
  (+) (E a) (E b) = E (BinOp (newLabel ()) Add a b)
  (*) (E a) (E b) = E (BinOp (newLabel ()) Mul a b)

  abs = undefined 
  signum = undefined 
  
  fromInteger a = (E (Lit (fromInteger a)))

instance Eq Exp where 
  (==) = undefined -- compare labels here

instance Num Exp where 
  (+) (a) (b) = BinOp (newLabel ()) Add a b
  (*) (a) (b) = BinOp (newLabel ()) Mul a b

  abs = undefined 
  signum = undefined 
  
  fromInteger a = Lit$ fromInteger a


------------------------------------------------------------------------------
-- Dimensions 
  
{- Support only one, two and threeD arrays, as in ArBB -} 
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

------------------------------------------------------------------------------
-- The AST                       
data Arr a = Arr Id                       
                      
data AC a where   
   ACInput     :: V.Vector e -> AC (Arr e)
   ACAddReduce :: AC (Arr e) -> AC (Arr e)
   ACMulReduce :: AC (Arr e) -> AC (Arr e) 
   
   -- TODO: encode function type also 
   -- TODO: the different input arrays can potentially have different type
   ACMap       :: Function -> [AC (Arr e)] -> AC (Arr e)
   ACSort      :: AC (Arr e) -> AC (Arr e)
   
type ACVector = AC (Arr Int32)
             

{- User level arrays. currently just a list of elements and a shape descriptor -}                   
data UserArray = UA {dimensions :: Dim, 
                     contents   :: [Int32]}
               deriving Show 

                  
-- This should be something like 
--   MyState a   = State (Int,Map.Map Int SomeFixedArrayRepresentation) a
type MyState b a = State (Int,Map.Map Int b) a

-- Just a thought 
data SomeFixedArrayRepresentation = SFAR (Ptr ()) Dim ArBB.ScalarType

-- with (MyState a) as above, Storable has methods to turn "things" into "SomeFixedArrayRepresentation"

--class StorableAC b where 
--  inputAC :: b -> MyState b (AC (Arr Int32))



getID :: MyState b Int 
getID = get >>= (return . fst) 

getMap :: MyState b (Map.Map Int b) 
getMap = get >>= (return . snd) 

putID :: Int -> MyState b () 
putID i = do   
  (_,m) <- get
  put (i,m) 
  
putMap :: Map.Map Int b -> MyState b ()
putMap m = do 
  (i,_) <- get
  put (i,m)
         
runMyState a = runState a (0,Map.empty)

-- for use with the interpreter

--instance StorableAC UserArray where 
--  inputAC arr = do 
--    id <- getID
--    m  <- getMap 
--    putMap (Map.insert id arr m) 
--    putID  (id+1);
--    return$ ACInput (Arr id)



------------------------------------------------------------------------------
-- Few Examples
toBinFunction f = f (E (Var (newLabel ()) 0)) (E (Var (newLabel ()) 1))
expFunToBinFun f = Lam (Lam (E (f (Var (newLabel ()) 0) (Var (newLabel ()) 1))))
expFunToUnFun  f = Lam (E (f (Var (newLabel ()) 0)))    
                   

-- evalOp op a b = evalExp (op (Lit a) (Lit b)) 

evalExp (Lit a) = a 
evalExp (Var l i) = -1 -- error "evalExp: variables not yet implemented" 
evalExp (BinOp l op e1 e2) = evalBinOp op (evalExp e1) (evalExp e2) 
-- evalExp (e1 :*: e2) = (evalExp e1) * (evalExp e2)
--evalExp (Index0 l a) = error "evalExp: Indexing not yet implemented"
evalExp _ = error "evalExp: not yet implemented" 

evalBinOp Add a1 a2 = a1 + a2 
evalBinOp Mul a1 a2 = a1 * a2
evalBinOp Sub a1 a2 = a1 - a2 

-- generalMap: correct for 1D,2D,3D. 
generalMap :: Function -> [[Int32]] -> [Function] 
generalMap f xs | nub xs ==  [[]] = [] 
generalMap f xs = applyList f (map (E . Lit . head) xs) : 
                  generalMap f (map tail xs)  


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



--run :: MyState UserArray Array -> UserArray
--run prg = let (a,(i,m)) = runMyState prg
--          in  eval a m



{- Examples 

run (Garb.sum (UA (dim1 10) [0..9]))

run (incr (UA (dim1 10) [0..9]))
-} 




------------------------------------------------------------------------------
-- ArBB Backend things

data ArBBArray = ArBBArray { dim     :: Dim,
                             eltType :: ArBB.ScalarType, 
                             var     :: ArBB.Variable }  
  
-----------------------------------------------------------------------------
-- Generate ArBB function from "Function" 
genFun :: Function -> ArBB.EmitArbb ArBB.ConvFunction   
genFun f = do 
  s <- ArBB.getScalarType_ ArBB.ArbbI32  -- out type (CHEAT) 
  let its = replicate (arity f) s        -- in  type (CHEAT)
  fun <- ArBB.funDef_ "generated" [s] its $ \ [out] inp -> do 
    r <- functionBody (body f) inp 
    ArBB.copy_ out r
   -- ArBB.op_ ArBB.ArbbOpMul out inp
  return fun
         
    
-- Expression to function body, 
-- Any "Arrays" that may occur here will be "global".
--  This will at least be true while "Map" is the only way 
--  to call a function. 
functionBody :: Exp -> [ArBB.Variable] -> ArBB.EmitArbb ArBB.Variable
functionBody (Lit i) _ = ArBB.int32_ i 
functionBody (Var l ix) env = return (env !! ix) 

-- TODO: Bit repetitive! fix. 
functionBody (BinOp l Add e1 e2) env = do  
  v1 <- functionBody e1 env 
  v2 <- functionBody e2 env 
  s  <- ArBB.getScalarType_ ArBB.ArbbI32
  r  <- ArBB.createLocal_ s "res"
  ArBB.op_ ArBB.ArbbOpAdd [r] [v1,v2]
  return r
functionBody (BinOp l Mul e1 e2) env = do   
  v1 <- functionBody e1 env 
  v2 <- functionBody e2 env 
  s  <- ArBB.getScalarType_ ArBB.ArbbI32
  r  <- ArBB.createLocal_ s "res"
  ArBB.op_ ArBB.ArbbOpMul [r] [v1,v2]
  return r

-- Index into arrays (This is broken!)
--functionBody (Index0 l arr) _ = undefined
--functionBody (Index1 l arr e1) _ = undefined 
--functionBody (Index2 l arr e1 e2) _ = undefined 
--functionBody (Index3 l arr e1 e2 e3) _ = undefined 


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
-- Evaluate   

evalAC :: ACVector -> UserArray 
evalAC (ACInput vector) = let dat = V.toList vector 
                          in UA (One (length dat)) dat
evalAC (ACAddReduce ua) = fold (+) 0 (evalAC ua)
evalAC (ACMulReduce ua) = fold (*) 1 (evalAC ua) 

evalAC (ACMap f uas) = UA (dimensions (head inputs)) (fixup (generalMap f inputData))                                                                   
  where inputs = map (\x -> evalAC x) uas
        inputData = map contents inputs 
evalAC (ACSort a) = UA (dimensions a') (List.sort (contents a'))
  where 
    a' = evalAC a  

 
fixup = map (\(E e) -> evalExp e)        

       

--runArBB_AC :: MyState UserArray (AC (Arr Int32)) -> IO UserArray 
--runArBB_AC prg = let (a, (i,m)) = runMyState prg
--                 in  ArBB.arbbSession$ do m' <- uploadArrays m 
--                                          res <- evalArBBImm_AC a m' 
--                                          readBack res
                                          
        
runArBB :: ACVector -> V.Vector Int32
runArBB prg = do 
   unsafePerformIO$ ArBB.arbbSession$ 
     do res <- evalArBBImm_AC prg 
        readBackVector res

        
evalArBBImm_AC :: ACVector  -> ArBB.EmitArbb ArBBArray
evalArBBImm_AC (ACInput vector)  = uploadArrayVector vector 
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
sortAC :: V.Vector Int32 -> ACVector
sortAC as =  
  let i = ACInput as
  in ACSort i
    
testSortAC = evalAC arr 
  where 
    arr = Garb.sortAC (V.fromList [7,6,5,4,3,2,1,0])

testSortArBB =
  ArBB.arbbSession$ do res <- evalArBBImm_AC a -- m' 
                       readBack res
  where 
    a = Garb.sortAC (V.fromList ((reverse [0..4095])++[0..4095]))