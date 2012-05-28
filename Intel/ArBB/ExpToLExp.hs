
{- This module is a hack !
   The step this performs should be removed (skipped over) -} 

module Intel.ArBB.ExpToLExp ( expToLExp 
                            , labelExps) where 

import Intel.ArBB.Syntax

import System.Mem.StableName 
import System.IO.Unsafe

import qualified Data.Map as M
import Data.Word

import Control.Monad.State hiding (liftIO) 

type LabelMap = M.Map Int (Word32,StableName Expr) 

type Labeler a = StateT (Word32, LabelMap)  IO a 

 
----------------------------------------------------------------------------
newLabel :: Labeler Word32
newLabel = do 
  (l,s) <- get 
  put (l+1,s) 
  return l 

----------------------------------------------------------------------------

liftIO :: IO a -> Labeler a 
liftIO = lift 


----------------------------------------------------------------------------

addExp :: Expr -> Labeler Word32
addExp e = 
    do
      (l,s) <- get 
      nom <- liftIO$ makeStableName e
      let hnom = hashStableName nom
      case M.lookup hnom s of 
        Nothing -> 
            do
              --liftIO$ putStrLn "not in map" 
              -- TODO: Can you use the hash directly without using a map at all. 
              --        In this hack probably, yes. 
              let s' = M.insert hnom (l,nom) s
              put (l+1,s') 
              return l 
        (Just (l,sn)) -> 
            do
              --liftIO$ putStrLn "FOUND!"
              when (sn /= nom) (error "same hash but stablenames mismatch")
              return l 

----------------------------------------------------------------------------
label :: Expr -> Labeler LExp 
label e@(Lit v) =
    do 
      l <- addExp e 
      return (LLit l v) 
label e@(Var v) = 
    do 
      l <- addExp e 
      return (LVar l v) 
label e@(Index0 exp) = 
    do
      l <- addExp e 
      exp' <- label exp
      return (LIndex0 l exp') 
label e@(ResIndex exp i) = 
    do 
      l <- addExp e 
      exp' <- label exp
      return (LResIndex l exp' i) 
label e@(Call fn exprs) = 
    do  
      l <- addExp e 
      exprs' <- mapM label exprs 
      return (LCall l fn exprs')
label e@(Map fn exprs) = 
    do  
      l <- addExp e 
      exprs' <- mapM label exprs 
      return (LMap l fn exprs')


label e@(While cond body init_state) = 
    do
      l <- addExp e 
      variables <- createVariables init_state
      
      let vexps = map Var variables
          cond' = cond vexps 
          body' = body vexps 
      i' <- mapM label init_state
      c' <- label cond'
      b' <- mapM label body' 

      return $ LWhile l variables c' b' i'
    
label e@(If e1 e2 e3) = 
    do 
      l <- addExp e 
      e1' <- label e1
      e2' <- label e2 
      e3' <- label e3 
      return (LIf l e1' e2' e3') 
label e@(Op op exprs) = 
    do
      l <- addExp e 
      exprs' <- mapM label exprs
      return (LOp l op exprs') 

createVariables :: [Expr] -> Labeler [Variable] 
createVariables [] = return []
createVariables (x:xs) = do 
  l <- newLabel 
  let v = Variable ("l"++show l)
  vs <- createVariables xs 
  return (v:vs) 
  


----------------------------------------------------------------------------
expToLExp :: Expr -> LExp 
expToLExp e = fst$ unsafePerformIO$ runStateT (label e) (0,M.empty)

labelExps :: [Expr] -> IO [LExp] 
labelExps es = liftM fst (labelExps' es (0,M.empty))
    where 
      labelExps' [] s = return ([],s)
      labelExps' (x:xs) s = 
          do 
            --putStrLn $ "labelExps'0"
            (x',s') <- runStateT (label x) s 
            --putStrLn "labelExps'1"
            (xs',s'') <- labelExps' xs s'
            --putStrLn "labelExps'2"
            return (x':xs',s')