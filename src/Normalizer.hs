module Normalizer where

import AST

splitAssumptions :: Expr -> ([Expr], Expr)
splitAssumptions (Imply p (Imply p' q)) = (ps ++ ps' ++ qs, q')
  where ps = conjuctAssumptions p
        ps' = conjuctAssumptions p'
        (qs, q') = splitAssumptions q
splitAssumptions (Imply p q) = (conjuctAssumptions p, q)
splitAssumptions p = ([], p)

conjuctAssumptions :: Expr -> [Expr]
conjuctAssumptions (Imply p q) = p : conjuctAssumptions q
conjuctAssumptions p = [p]
