module Normalizer where

import AST

splitAssumptions :: Expr -> ([Expr], Expr)
splitAssumptions (Imply p q) = ([], LitBool True)
splitAssumptions _ = error "splitAssumtions expexts an Imply"
