module Renaming where

import Control.Monad.State
import Data.List (elemIndex, findIndex)
import AST

rename :: Stmt -> State Int Stmt
rename Skip = return Skip
rename (Assume e) = do
  e' <- renameE e
  return $ Assume e'
rename (Assert e) = do
  e' <- renameE e
  return $ Assert e'
rename (Seq s1 s2) = do
  s1' <- rename s1
  s2' <- rename s2
  return $ Seq s1' s2'
rename (Asg targets exprs) = do
  exprs' <- mapM renameE exprs
  return $ Asg targets exprs'
rename (VarStmt targets body) = do
  body' <- rename body
  counter <- get
  let len = length targets
  put (counter + len)
  let targets' = ["TEMP_" ++ show i | i <- [counter..(counter + len - 1)]]
  return $ VarStmt targets' (subst targets targets' body')
rename _ = error "rename does not accept branching statements"

rename1 cons e = do
  e' <- renameE e
  return $ cons e'
rename2 cons e1 e2 = do
  e1' <- renameE e1
  e2' <- renameE e2
  return $ cons e1' e2'
rename3 cons e1 e2 e3 = do
  e1' <- renameE e1
  e2' <- renameE e2
  e3' <- renameE e3
  return $ cons e1' e2' e3'
renameE :: Expr -> State Int Expr
renameE (Plus e1 e2) = rename2 Plus e1 e2
renameE (Minus e1 e2) = rename2 Minus e1 e2
renameE (Imply e1 e2) = rename2 Imply e1 e2
renameE (Lt e1 e2) = rename2 Lt e1 e2
renameE (Eq e1 e2) = rename2 Eq e1 e2
renameE (ArrayAccess v e) = rename1 (ArrayAccess v) e
renameE (Cond g et ef) = rename3 Cond g et ef
renameE (RepBy arr i e) = rename3 RepBy arr i e
renameE (Forall (BVar v typ) e) = do
  counter <- get
  put (counter + 1)
  let v' = "TEMP_" ++ show counter
  e' <- renameE $ substE [v] [v'] e
  return $ Forall (BVar v' typ) e'
renameE e = return e

subst :: [String] -> [String] -> Stmt -> Stmt
subst _ _ Skip = Skip
subst ts es (Assume e) = Assume $ substE ts es e
subst ts es (Assert e) = Assert $ substE ts es e
subst ts es (Seq s1 s2) = Seq (subst ts es s1) (subst ts es s2)
subst ts es (Asg targets exprs) =
  Asg (map substVars targets) (map (substE ts es) exprs)
  where substVars s = case elemIndex s ts of
                        Just i -> es !! i
                        Nothing -> s
subst ts es (VarStmt targets body) =
  VarStmt targets body'
  where body' = subst ts' es' body
        (ts', es') = unzip $ filter (\(t, _) -> notElem t targets) (zip ts es)
subst _ _ _ = error "subst does not accept branching statements"

substE :: [String] -> [String] -> Expr -> Expr
substE ts es (Plus e1 e2) = Plus (substE ts es e1) (substE ts es e2)
substE ts es (Minus e1 e2) = Minus (substE ts es e1) (substE ts es e2)
substE ts es (Imply e1 e2) = Imply (substE ts es e1) (substE ts es e2)
substE ts es (Lt e1 e2) = Lt (substE ts es e1) (substE ts es e2)
substE ts es (Eq e1 e2) = Eq (substE ts es e1) (substE ts es e2)
substE ts es (ArrayAccess v e) = ArrayAccess v (substE ts es e)
substE ts es (Cond g et ef) =
  Cond (substE ts es g) (substE ts es et) (substE ts es ef)
substE ts es (RepBy arr i e) =
  RepBy (substE ts es arr) (substE ts es i) (substE ts es e)
substE ts es n@(Name x) =
  case elemIndex x ts of
    Just i -> Name $ es !! i
    Nothing -> n
substE ts es (Forall (BVar v typ) e) =
  Forall (BVar v typ) e'
  where e' = substE ts' es' e
        (ts', es') = unzip $ filter (\(t, _) -> t /= v) (zip ts es)
substE _ _ q = q
