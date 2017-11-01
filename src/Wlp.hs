module Wlp where

import Data.List (elemIndex, nub)
import qualified Data.Map as M

import AST
import Debug.Trace


wlp :: Stmt -> Expr -> Expr
wlp Skip q = q
wlp s@(Assert a) q =
  case q of
    Forall ["$goal"] q ->
      markGoal $ a /\ q
    (BinOp Imply p@(Forall ["$assumption"] _) q) ->
      p ==> wlp s q
    (LitBool True) ->
      a
    _ -> error $ "Assert on " ++ show q
wlp (Assume p) q = markAssumption p ==> q
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
        e -> error $ "subst to array expect repby: got " ++ show e ++ "\n" ++ show (zip ts es)
    Nothing -> access'
  where
    index' = subst ts es index
    access' = ArrayAccess arr index'
subst ts es (RepBy arr i e) =
  RepBy (subst ts es arr) (subst ts es i) (subst ts es e)
subst _ _ q = q
