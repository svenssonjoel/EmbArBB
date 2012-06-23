{-# LANGUAGE GeneralizedNewtypeDeriving, 
             MultiParamTypeClasses, 
             FlexibleInstances, 
             ScopedTypeVariables,
             TypeOperators #-}

module Intel.ArBB.Reify where 

import Control.Monad.State.Strict
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Dynamic as Dynamic hiding (typeOf) 

import System.Mem.StableName


import Intel.ArBB.DAG
import Intel.ArBB.Types 
import Intel.ArBB.Syntax
import Intel.ArBB.Variable
import Intel.ArBB.Data
import Intel.ArBB.Vector
import Intel.ArBB.ReifyableType

import Intel.ArBB.MonadReify
import Intel.ArBB.GenRecord
 
import Data.Int
import Data.Word
import Data.Hashable 

import Intel.ArBB.Data.Int

----------------------------------------------------------------------------
-- Create GenRecords from functions.
----------------------------------------------------------------------------


---------------------------------------------------------------------------- 
-- Implement operations on the Capture monad. 
-- These should all be backend oblivious. 
newVar :: R Variable 
newVar = do 
  uniq <- newUnique 
  return $ Variable ("v" ++ show uniq)

newUnique :: R Integer
newUnique = do 
  uniq <- gets unique 
  modify $ \s -> s {unique = uniq+1}
  return uniq

addVarType :: Variable -> Type -> R () 
addVarType v t = do 
  m <- gets (genRecordVarType . genRecord)
  modify $ \(RState sh uniq gr)  -> RState sh uniq gr { genRecordVarType = Map.insert v t m } 

-- TODO: Break up into tiny functions. 
-- TODO: I dont really need the Dynamic right ?
getNodeID :: Expr -> R NodeID
getNodeID e = 
    do
      sh <- gets sharing  
      sn <- liftIO$ makeStableName e 
      let hsn = hashStableName sn 
            
      case IntMap.lookup hsn sh of 
        (Just hits) -> case lookup (Just sn) [(fromDynamic d,i)| (d,i) <- hits] of 
                         (Just n) -> 
                             do 
                               --liftIO $ putStr "Already computed: " 
                               --liftIO $ putStrLn $ show e 
                               return n -- Already computed
                         Nothing  -> 
                             do 
                               --liftIO $ putStr "Hit but inserts: "
                               --liftIO $ putStrLn $ show e 
                               uniq <- gets unique 
                               modify $ \s -> s { sharing = IntMap.insert hsn ((toDyn sn,uniq):hits) sh} 
                               modify $ \s -> s { unique = uniq + 1}
                               return uniq
        Nothing     -> 
            do
              --liftIO $ putStr "No Hit! inserts: "
              --liftIO $ putStrLn $ show e 
              uniq <- gets unique
              modify $ \s -> s {sharing = IntMap.insert hsn [(toDyn sn,uniq)] sh}
              modify $ \s -> s {unique = uniq + 1}
              return uniq


insertNode :: Expr -> Node -> R GenRecord 
insertNode e node = 
    do 
      d <- gets (genRecordDag . genRecord)
      nid <- getNodeID e 
      let d' = Map.insert nid node d 
      modify $ \(RState sh uniq gr) -> RState sh uniq (gr { genRecordDag = d'})
      
      
      modify $ \(RState sh uniq gr) -> RState sh uniq (gr {genRecordNids = [nid]})
      gets genRecord
----------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------- 


                         
                        
class Reify a where 
    reify :: a -> R GenRecord 

runR r = evalStateT r capState 
    where 
      capState = 
         RState IntMap.empty 
                   0
                   emptyGenRecord -- Map.empty

reifySimple :: Expr -> R [NodeID] 
reifySimple e = 
    do 
      gr <- reify e 
      return (genRecordNids gr) -- gets (genRecordNids . genRecord)


reifyLocally :: Expr -> R [NodeID] 
reifyLocally e = 
    do 
      -- get all state 
      (RState sh un gr) <- get 
                              
      nids <- reifySimple e

      -- restore sharing to as it was before. 
      -- Should prevent anything "local" to be seen in future sharing detection.. 
      -- TODO: Make sure this works
      modify $ \s -> s { sharing = sh }
      return nids
                              
      
    
---------------------------------------------------------------------------- 
-- 
instance Reify Expr where 
    reify e@(Var v) = insertNode e (NVar v) 
    reify e@(Lit l) = insertNode e (NLit l)
    reify e@(Index0 exp) = 
        do
          [exp'] <- reifySimple exp 
          insertNode e (NIndex0 exp')

    reify e@(ResIndex exp i) = 
        do 
          [exp'] <- reifySimple exp 
          insertNode e (NResIndex exp' i) 

    -- DONE: CALL and MAP needs to change a lot (future work)    
    reify e@(Call cap exprs) = 
        do
          -- This part is messed up!
          imm <- mapM reify exprs
          let exprs' = map genRecordNids imm
          -- -----------------------
          
          depend <- lift $ runR cap
          uniq <- newUnique 
          
          depends <- gets (genRecordDepends . genRecord) 
          let depends' = Map.insert uniq depend depends
          modify $ \(RState sh un gr) -> RState sh un (gr { genRecordDepends = depends' }) 
          insertNode e (NMap uniq (concat exprs'))
           
          --fid <- lift (runR cap) 
          --exprs' <- mapM reify exprs 
          --insertNode e (NCall fid (concat exprs'))  --Concat... make sure this is as it shall

    reify e@(Map cap exprs) = 
        do
          
          -- This part is messed up!
          imm <- mapM reify exprs
          let exprs' = map genRecordNids imm
          -- -----------------------
          
          depend <- lift $ runR cap
          uniq <- newUnique 
          
          depends <- gets (genRecordDepends . genRecord) 
          let depends' = Map.insert uniq depend depends
          modify $ \(RState sh un gr) -> RState sh un (gr { genRecordDepends = depends' }) 
          insertNode e (NMap uniq (concat exprs'))
                    
          --fid  <- lift ( runR cap )
          --liftIO$ putStrLn $ "generated map fun has id: " ++ show fid
          --exprs' <- mapM reify exprs 
          --insertNode e (NMap fid (concat exprs'))
                    
    reify e@(If e1 e2 e3) = 
        do
          [e1'] <- reifySimple e1
          [e2'] <- reifySimple e2
          [e3'] <- reifySimple e3
          insertNode e (NIf e1' e2' e3')

    ----------------------------------------------------
    -- While Loop 
    ----------------------------------------------------
    reify e@(While cond body state) = 
        do 
          variables <- createVariables state 
          
          let vexps = map Var variables 
              cond' = cond vexps 
              body' = body vexps 
          i' <- mapM reifySimple state
   
          -- TODO: I am very unsure here. Is this right ? 
          [c'] <- reifyLocally cond'
          b'   <- mapM reifyLocally body'
          insertNode e (NWhile variables c' (concat b') (concat i')) 

    reify e@(Op op exprs) = 
        do
          exprs' <- mapM reifySimple exprs 
          insertNode e (NOp op (concat exprs')) 

----------------------------------------------------------------------------
-- 

createVariables :: [Expr] -> R [Variable]
createVariables [] = return []
createVariables (x:xs) = 
    do 
      uniq <- newUnique 
      let v = Variable ("l"++show uniq)
      vs <- createVariables xs
      return (v:vs)

---------------------------------------------------------------------------- 
-- 
      
instance Reify (Exp a) where 
    reify = reify . unE 

 
instance (ReifyableFunType a b, ReifyableFun a b) => Reify (a -> b) where 
    reify f = 
        do
          exprs <- reifyFun f
          nids <- mapM reifySimple exprs
          modify $ \(RState sh un gr) -> RState sh un ( gr {genRecordNids = (concat nids)})
          modify $ \(RState sh un gr) -> RState sh un ( gr {genRecordFunType = reifyFunType f})

          dag <- gets (genRecordDag . genRecord) 
          
          let h = hash dag 
          
          modify $ \(RState sh un gr) -> RState sh un ( gr {genRecordHash = h}) 

          gets genRecord

----------------------------------------------------------------------------
-- 
class ReifyableFun a b where 
    reifyFun :: (a -> b) -> R [Expr]


instance Data a => ReifyableFun (Exp a) (Exp b) where 
    reifyFun f = 
        do 
          v <- newVar 
          let t = typeOf (undefined :: a) 
          addVarType v t 
          let e = f $ E (Var v)  
                     
          return [unE e]

instance Data a => ReifyableFun (Exp a) (Exp b,Exp c) where 
    reifyFun f = 
        do 
          v <- newVar 
          let t = typeOf (undefined :: a) 
          addVarType v t 
          let (e1,e2) = f $ E (Var v)  
                     
          return [unE e1,unE e2]

instance Data a => ReifyableFun (Exp a) (Exp b,Exp c, Exp d) where 
    reifyFun f = 
        do 
          v <- newVar 
          let t = typeOf (undefined :: a) 
          addVarType v t 
          let (e1,e2,e3) = f $ E (Var v)  
                     
          return [unE e1,unE e2,unE e3]


instance (Data a, ReifyableFun b c ) => ReifyableFun (Exp a)  (b -> c) where 
    reifyFun f = 
        do 
          v <- newVar 
          let t = typeOf (undefined :: a)
          addVarType v t 
          reifyFun $ f (E (Var v))
