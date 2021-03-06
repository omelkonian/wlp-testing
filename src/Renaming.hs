module Renaming (rename, prefix) where

import Control.Monad.State
import Data.List (elemIndex, findIndex)
import Data.Maybe (fromMaybe)

import AST

-- | Prefix of internally-generated variables.
prefix = '$'

-- | Rename local variables in `Var` statements with fresh names.
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
  return $ s1' <:> s2'
rename (Asg targets exprs) = do
  exprs' <- mapM renameE exprs
  return $ targets .:= exprs'
rename (VarStmt targets body) = do
  body' <- rename body
  counter <- get
  let len = length targets
  put (counter + len)
  let targets' = [prefix : show i | i <- [counter..(counter + len - 1)]]
  return $ VarStmt targets' (subst targets targets' body')
rename _ = error "rename does not accept branching statements"

-- | Rename local variables in quantified expressions with fresh names.
renameE :: Expr -> State Int Expr
renameE (Not e) = rename1 Not e
renameE (BinOp op e1 e2) = do
  e1' <- renameE e1
  e2' <- renameE e2
  return $ BinOp op e1' e2'
renameE (Cond g et ef) = rename3 Cond g et ef
renameE (Forall v e) = renameSubE Forall v e
renameE (Exist v e) = renameSubE Exist v e
renameE (ArrayAccess v e) = rename1 (ArrayAccess v) e
renameE (RepBy arr i e) = rename3 RepBy arr i e
renameE e = return e

rename1 cons e = do
  e' <- renameE e
  return $ cons e'
rename3 cons e1 e2 e3 = do
  e1' <- renameE e1
  e2' <- renameE e2
  e3' <- renameE e3
  return $ cons e1' e2' e3'

renameSubE cons v e = do
  counter <- get
  let n = length v
  if head (head v) == prefix then do
    e' <- renameE e
    return $ cons v e'
  else do
    put (counter + n)
    let v' = [prefix : show c | c <- [counter..(counter + n - 1)]]
    e' <- renameE $ substE v v' e
    return $ cons v' e'

-- | Replace renamed variables in inner statements.
subst :: [String] -> [String] -> Stmt -> Stmt
subst _ _ Skip = Skip
subst ts es (Assume e) = Assume $ substE ts es e
subst ts es (Assert e) = Assert $ substE ts es e
subst ts es (Seq s1 s2) = subst ts es s1 <:> subst ts es s2
subst ts es (Asg targets exprs) =
  map substVars targets .:= map (substE ts es) exprs
  where substVars s = case elemIndex s ts of
                        Just i -> es !! i
                        Nothing -> s
subst ts es (VarStmt targets body) =
  VarStmt targets body'
  where body' = subst ts' es' body
        (ts', es') = unzip $ filter (\(t, _) -> notElem t targets) (zip ts es)
subst _ _ _ = error "subst does not accept branching statements"

-- | Replace renamed variables in inner expressions.
substE :: [String] -> [String] -> Expr -> Expr
substE ts es n@(Name x) =
  case elemIndex x ts of
    Just i -> Name $ es !! i
    Nothing -> n
substE ts es (Not e) = Not $ substE ts es e
substE ts es (BinOp op e1 e2) =
  BinOp op (substE ts es e1) (substE ts es e2)
substE ts es (Cond g et ef) =
  Cond (substE ts es g) (substE ts es et) (substE ts es ef)
substE ts es (Forall vs e) = substSubE ts es Forall vs e
substE ts es (Exist vs e) = substSubE ts es Exist vs e
substE ts es (ArrayAccess v e) =
  ArrayAccess v' e'
  where
    e' = substE ts es e
    v' = case elemIndex v ts of
           Just i -> es !! i
           Nothing -> v
substE ts es (RepBy arr i e) =
  RepBy (substE ts es arr) (substE ts es i) (substE ts es e)
substE _ _ q = q

substSubE ts es cons vs e =
  cons vs e'
  where e' = substE ts' es' e
        (ts', es') = unzip $ filter (\(t, _) -> t `notElem` vs) (zip ts es)
