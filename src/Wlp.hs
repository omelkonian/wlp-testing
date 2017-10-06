module Wlp where

import Data.List (elemIndex)
import AST

wlp :: Stmt -> Expr -> Expr
wlp Skip q = q
wlp (Assert p) q =
  case q of
    LitBool True -> p
    _ -> and_ p q
wlp (Assume p) q =
  case p of
    LitBool True -> q
    _ -> Imply p q
wlp (Seq s1 s2) q = wlp s1 (wlp s2 q)
wlp (Asg targets exprs) q = subst targets exprs q
wlp (VarStmt targets body) q = wlp body q
wlp _ _ = error "wlp expects non-branching statements"

subst :: [String] -> [Expr] -> Expr -> Expr
subst ts es (Plus e1 e2) = Plus (subst ts es e1) (subst ts es e2)
subst ts es (Minus e1 e2) = Minus (subst ts es e1) (subst ts es e2)
subst ts es (Imply e1 e2) = Imply (subst ts es e1) (subst ts es e2)
subst ts es (Lt e1 e2) = Lt (subst ts es e1) (subst ts es e2)
subst ts es (Eq e1 e2) = Eq (subst ts es e1) (subst ts es e2)
subst ts es (ArrayAccess v e) = ArrayAccess v (subst ts es e)
subst ts es (Cond g et ef) =
  Cond (subst ts es g) (subst ts es et) (subst ts es ef)
subst ts es (RepBy arr i e) =
  RepBy (subst ts es arr) (subst ts es i) (subst ts es e)
subst ts es (Name x) =
  case elemIndex x ts of
    Just i -> es !! i
    Nothing -> Name x
subst ts es (Forall (BVar v typ) e) =
  Forall (BVar v typ) e'
  where e' = subst ts' es' e
        (ts', es') = unzip $ filter (\(t, _) -> t /= v) (zip ts es)
subst _ _ q = q
