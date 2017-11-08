module Normalizer (normalize, prenexFixpoint, stripMarks) where

import Control.Applicative ((<|>))
import Data.List

import AST

-- | Normalize a logical expression to a list of assumptions and a goal.
normalize :: Expr -> ([Expr], Maybe Expr)
normalize (BinOp Imply (Forall ["$assumption"] p) (Forall ["$goal"] g)) =
  ([p], Just g)
normalize (BinOp Imply (Forall ["$assumption"] p) e) =
  let (assumptions, goal) = normalize e
  in (p : assumptions, goal)
normalize _ = ([], Nothing)

-- | Convert a logical expression to Prenex Normal form.
prenexFixpoint :: Expr -> Expr
prenexFixpoint e =
  if e' == e then groupQuantifiers e' else prenexFixpoint e'
  where e' = prenex e
prenex :: Expr -> Expr
prenex (Not (Forall vs e)) =
  Exist vs $ Not e
prenex (Not (Exist vs e)) =
  Forall vs $ Not e
prenex (BinOp Imply (Forall vs p) q) =
  Exist vs $ prenex p ==> prenex q
prenex (BinOp Imply (Exist vs p) q) =
  Forall vs $ prenex p ==> prenex q
prenex (BinOp Imply p (Forall vs q)) =
  Forall vs $ prenex p ==> prenex q
prenex (BinOp Imply p (Exist vs q)) =
  Exist vs $ prenex p ==> prenex q
prenex (BinOp And (Forall vs e) (Forall vs' e')) =
  Forall (vs ++ vs') $ prenex e /\ prenex e'
prenex (BinOp And (Forall vs e) (Exist vs' e')) =
  Forall vs $ Exist vs' $ prenex e /\ prenex e'
prenex (BinOp And (Exist vs e) (Exist vs' e')) =
  Exist (vs ++ vs') $ prenex e /\ prenex e'
prenex (BinOp And (Exist vs e) (Forall vs' e')) =
  Forall vs' $ Exist vs $ prenex e /\ prenex e'
prenex (BinOp And (Forall vs e) e') =
  Forall vs $ prenex e /\ prenex e'
prenex (BinOp And e (Forall vs e')) =
  Forall vs $ prenex e /\ prenex e'
prenex (BinOp And (Exist vs e) e') =
  Exist vs $ prenex e /\ prenex e'
prenex (BinOp And e (Exist vs e')) =
  Exist vs $ prenex e /\ prenex e'
prenex (BinOp op p q) =
  BinOp op (prenex p) (prenex q)
prenex (Forall vs e) =
  Forall vs $ prenex e
prenex (Exist vs e) =
  Exist vs $ prenex e
prenex e = e

-- | Group all quantifiers of an expression in Prenex Normal form to a single
-- universal quantifier and a single existential quantifier.
groupQuantifiers :: Expr -> Expr
groupQuantifiers e =
  let (e', un, ex) = groupQuantifiers' e
      ex' = ex \\ un
  in case (un, ex') of
      ([], []) -> e'
      ([], ex) -> Exist ex e'
      (un, []) -> Forall un e'
      (un, ex) -> Forall un $ Exist ex e'
groupQuantifiers' :: Expr -> (Expr, [String], [String])
groupQuantifiers' (Forall vs e) =
  let (e', un, ex) = groupQuantifiers' e
  in (e', vs ++ un, ex)
groupQuantifiers' (Exist vs e) =
  let (e', un, ex) = groupQuantifiers' e
  in (e', un, vs ++ ex)
groupQuantifiers' e = (e, [], [])

-- | Remove marks for assumptions and goal.
stripMarks :: Expr -> Expr
stripMarks f@(Forall [mark] e) =
  if mark `elem` ["$assumption", "$goal"] then e else f
stripMarks (Not e) = Not $ stripMarks e
stripMarks (BinOp op e e') = BinOp op (stripMarks e) (stripMarks e')
stripMarks (Cond g et ef) = Cond g (stripMarks et) (stripMarks ef)
stripMarks (ArrayAccess a e) = ArrayAccess a (stripMarks e)
stripMarks (RepBy a i e) = RepBy (stripMarks a) (stripMarks i) (stripMarks e)
stripMarks e = e
