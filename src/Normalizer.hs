module Normalizer where

import Control.Applicative ((<|>))

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
