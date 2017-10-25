module Normalizer where

import AST

splitAssumptions :: Expr -> ([Expr], Expr)
splitAssumptions (BinOp Imply p (BinOp Imply p' q)) = (ps ++ ps' ++ qs, q')
  where ps = conjuctAssumptions p
        ps' = conjuctAssumptions p'
        (qs, q') = splitAssumptions q
splitAssumptions (BinOp Imply p q) = (conjuctAssumptions p, q)
splitAssumptions p = ([], p)

conjuctAssumptions :: Expr -> [Expr]
conjuctAssumptions (BinOp Imply p q) = p : conjuctAssumptions q
conjuctAssumptions p = [p]
