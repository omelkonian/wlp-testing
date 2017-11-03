module Normalizer where

import Control.Applicative ((<|>))
import Data.List

import AST

normalize :: Expr -> ([Expr], Maybe Expr)
normalize (BinOp Imply (Forall ["$assumption"] p) (Forall ["$goal"] g)) =
  ([p], Just g)
normalize (BinOp Imply (Forall ["$assumption"] p) e) =
  let (assumptions, goal) = normalize e
  in (p : assumptions, goal)
normalize _ = ([], Nothing)

stripMarks :: Expr -> Expr
stripMarks f@(Forall [mark] e) =
  if mark `elem` ["$assumption", "$goal"] then e else f
stripMarks (Not e) = Not $ stripMarks e
stripMarks (BinOp op e e') = BinOp op (stripMarks e) (stripMarks e')
stripMarks (Cond g et ef) = Cond g (stripMarks et) (stripMarks ef)
stripMarks (ArrayAccess a e) = ArrayAccess a (stripMarks e)
stripMarks (RepBy a i e) = RepBy (stripMarks a) (stripMarks i) (stripMarks e)
stripMarks e = e

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

toPrenexFormFixpoint :: Expr -> Expr
toPrenexFormFixpoint e =
  if e' == e then groupQuantifiers e' else toPrenexFormFixpoint e'
  where e' = toPrenexForm e

toPrenexForm :: Expr -> Expr
toPrenexForm (Not (Forall vs e)) =
  Exist vs $ Not e
toPrenexForm (Not (Exist vs e)) =
  Forall vs $ Not e
toPrenexForm (BinOp Imply (Forall vs p) q) =
  Exist vs $ toPrenexForm p ==> toPrenexForm q
toPrenexForm (BinOp Imply (Exist vs p) q) =
  Forall vs $ toPrenexForm p ==> toPrenexForm q
toPrenexForm (BinOp Imply p (Forall vs q)) =
  Forall vs $ toPrenexForm p ==> toPrenexForm q
toPrenexForm (BinOp Imply p (Exist vs q)) =
  Exist vs $ toPrenexForm p ==> toPrenexForm q
toPrenexForm (BinOp op p q) =
  BinOp op (toPrenexForm p) (toPrenexForm q)
toPrenexForm (Forall vs e) =
  Forall vs $ toPrenexForm e
toPrenexForm (Exist vs e) =
  Exist vs $ toPrenexForm e
toPrenexForm e = e
