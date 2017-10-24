module Wlp where

import Data.List (elemIndex, nub)
import qualified Data.Map as M

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
subst ts es (Not e) = Not (subst ts es e)
subst ts es (Lt e1 e2) = Lt (subst ts es e1) (subst ts es e2)
subst ts es (Eq e1 e2) = Eq (subst ts es e1) (subst ts es e2)
subst ts es (Cond g et ef) =
  Cond (subst ts es g) (subst ts es et) (subst ts es ef)
subst ts es (RepBy arr i e) =
  RepBy (subst ts es arr) (subst ts es i) (subst ts es e)
subst ts es (Name x) =
  case x `elemIndex` ts of
    Just i -> es !! i
    Nothing -> Name x
subst ts es (Forall vs e) =
  Forall vs e'
  where e' = subst ts' es' e
        (ts', es') = unzip $ filter (\(t, _) -> t `notElem` vs) (zip ts es)
subst ts es access@(ArrayAccess arr index) =
  case arr `elemIndex` ts of
    Just i ->
      case es !! i of
        RepBy a i e -> Cond (Eq index i) e access
        _ -> error "subst to array expect repby"
    Nothing -> access
subst _ _ q = q

getConds :: Expr -> [Expr]
getConds c@Cond {} = [c]
getConds (Plus e1 e2) = getConds e1 ++ getConds e2
getConds (Minus e1 e2) = getConds e1 ++ getConds e2
getConds (Imply e1 e2) = getConds e1 ++ getConds e2
getConds (Lt e1 e2) = getConds e1 ++ getConds e2
getConds (Eq e1 e2) = getConds e1 ++ getConds e2
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
  or_ (Imply g (replaceCond0 e (g, l))) (Imply (Not g) (replaceCond0 e (g, r)))

replaceCond0 :: Expr -> (Expr, Expr) -> Expr
replaceCond0 c@(Cond g _ _) (g', e') = if g == g' then e' else c
replaceCond0 (Plus e1 e2) c = Plus (replaceCond0 e1 c) (replaceCond0 e2 c)
replaceCond0 (Minus e1 e2) c = Minus (replaceCond0 e1 c) (replaceCond0 e2 c)
replaceCond0 (Imply e1 e2) c = Imply (replaceCond0 e1 c) (replaceCond0 e2 c)
replaceCond0 (Lt e1 e2) c = Lt (replaceCond0 e1 c) (replaceCond0 e2 c)
replaceCond0 (Eq e1 e2) c = Eq (replaceCond0 e1 c) (replaceCond0 e2 c)
replaceCond0 (Not e) c = Not (replaceCond0 e c)
replaceCond0 (ArrayAccess arr e) c = ArrayAccess arr (replaceCond0 e c)
replaceCond0 (Forall vs e) c = Forall vs (replaceCond0 e c)
replaceCond0 e _ = e


type Accesses = M.Map String [Expr]

uniqueUnion :: Accesses -> Accesses -> Accesses
uniqueUnion = M.unionWith (\l1 l2 -> nub $ l1 ++ l2)

accesses :: Expr -> Accesses
accesses (Plus e1 e2) = uniqueUnion (accesses e1) (accesses e2)
accesses (Minus e1 e2) = uniqueUnion (accesses e1) (accesses e2)
accesses (Lt e1 e2) = uniqueUnion (accesses e1) (accesses e2)
accesses (Eq e1 e2) = uniqueUnion (accesses e1) (accesses e2)
accesses (Imply e1 e2) = uniqueUnion (accesses e1) (accesses e2)
accesses (Not e) = accesses e
accesses (Forall _ e) = accesses e
accesses (ArrayAccess arr e) = uniqueUnion (M.singleton arr [e]) (accesses e)
accesses _ = M.empty
