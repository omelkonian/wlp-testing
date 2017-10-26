module Wlp where

import Data.List (elemIndex, nub)
import qualified Data.Map as M

import AST


wlp :: Stmt -> Expr -> Expr
wlp Skip q = q
wlp (Assert p) q = p /\ q
wlp (Assume p) q = p ==> q
wlp (Seq s1 s2) q = wlp s1 (wlp s2 q)
wlp (Asg targets exprs) q = subst targets exprs q
wlp (VarStmt targets body) q = wlp body q
wlp _ _ = error "wlp expects non-branching statements"

subst :: [String] -> [Expr] -> Expr -> Expr
subst ts es (Name x) =
  case x `elemIndex` ts of
    Just i -> es !! i
    Nothing -> Name x
subst ts es (Not e) = Not (subst ts es e)
subst ts es (BinOp op e1 e2) =
  BinOp op (subst ts es e1) (subst ts es e2)
subst ts es (Cond g et ef) =
  Cond (subst ts es g) (subst ts es et) (subst ts es ef)
subst ts es (Forall vs e) =
  Forall vs e'
  where e' = subst ts' es' e
        (ts', es') = unzip $ filter (\(t, _) -> t `notElem` vs) (zip ts es)
subst ts es (ArrayAccess arr index) =
  case arr `elemIndex` ts of
    Just i ->
      case es !! i of
        RepBy a i e -> Cond (index' .= i) e access'
        Name a' -> ArrayAccess a' index'
        _ -> error "subst to array expect repby"
    Nothing -> access'
  where
    index' = subst ts es index
    access' = ArrayAccess arr index'
subst ts es (RepBy arr i e) =
  RepBy (subst ts es arr) (subst ts es i) (subst ts es e)
subst _ _ q = q

getConds :: Expr -> [Expr]
getConds c@Cond {} = [c]
getConds (BinOp _ e1 e2) = getConds e1 ++ getConds e2
getConds (Not e) = getConds e
getConds (ArrayAccess _ e) = getConds e
getConds _ = []

fixpointReplaceConds :: Expr -> Expr
fixpointReplaceConds e =
  if null (getConds e) then e else fixpointReplaceConds (replaceConds e)

replaceConds :: Expr -> Expr
replaceConds e = foldl replaceCond e (getConds e)

replaceCond :: Expr -> Expr -> Expr
replaceCond e (Cond g l r) =
  (g ==> replaceCond0 e (g, l))
    /\
  (Not g ==> replaceCond0 e (g, r))

replaceCond0 :: Expr -> (Expr, Expr) -> Expr
replaceCond0 (Not e) c =
  Not (replaceCond0 e c)
replaceCond0 (BinOp op e1 e2) c =
  BinOp op (replaceCond0 e1 c) (replaceCond0 e2 c)
replaceCond0 c@(Cond g _ _) (g', e') =
  if g == g' then e' else c
replaceCond0 (Forall vs e) c =
  Forall vs (replaceCond0 e c)
replaceCond0 (ArrayAccess arr e) c =
  ArrayAccess arr (replaceCond0 e c)
replaceCond0 e _ = e
