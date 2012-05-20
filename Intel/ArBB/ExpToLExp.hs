
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

type LabelMap = M.Map Int Word32

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
              -- TODO: Can you use the hash directly without using a map at all. 
              --        In this hack probably, yes. 
              let s' = M.insert hnom l s
              put (l+1,s') 
              return l 
        (Just l) -> return l 

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
-- TODO: Figure this one out. 
label e@(While e1 es1 es2) = 
    do
      l <- addExp e 
      return $ error "While is not implemented" 

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
createVariables = undefined 


----------------------------------------------------------------------------
expToLExp :: Expr -> LExp 
expToLExp e = fst$ unsafePerformIO$ runStateT (label e) (0,M.empty)

labelExps :: [Expr] -> IO [LExp] 
labelExps es = liftM fst (labelExps' es (0,M.empty))
    where 
      labelExps' [] s = return ([],s)
      labelExps' (x:xs) s = 
          do 
            (x',s') <- runStateT (label x) s 
            (xs',s'') <- labelExps' xs s'
            return (x':xs',s')