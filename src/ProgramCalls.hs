module ProgramCalls (blackBox) where

import AST
import Renaming (prefix)
import Wlp (subst)

-- | Replace a statement's program calls with black-box testing conditions.
blackBox :: [Program] -> Stmt -> Stmt
blackBox ps a@(Asg targets es) =
  case head es of
    ProgCall progName es ->
      VarStmt fresh $
        freshIn .:= es
        <:> Assert (renew $ pre prog)
        <:> Assume (renew $ post prog)
        <:> targets .:= map n freshOut
      where
        prog = head $ filter (\p -> name p == progName) ps
        freshOut = map (\v -> prefix : v) (outputs prog)
        freshIn = map (\v -> prefix : v) (inputs prog)
        fresh = freshIn ++ freshOut
        old = inputs prog ++ outputs prog
        new = map n freshIn ++ map n freshOut
        renew = subst old new
    _ -> a
blackBox ps (Seq s s') =
  Seq (blackBox ps s) (blackBox ps s')
blackBox ps (Ite g s s') =
  Ite g (blackBox ps s) (blackBox ps s')
blackBox ps (While inv g s) =
  While inv g (blackBox ps s)
blackBox ps (VarStmt vs s) =
  VarStmt vs (blackBox ps s)
blackBox _ s = s
