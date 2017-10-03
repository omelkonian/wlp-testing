module Paths where

import Control.Monad
import Data.List (intersperse, subsequences, tails)
import AST

getPaths :: Int -> Stmt -> [Stmt]
getPaths d (VarStmt ids s) =
  [VarStmt ids s' | s' <- getPaths (d - 1) s]
getPaths d (Ite g st sf) = pathT ++ pathF
  where pathT = [Seq (Assume g) st' | st' <- getPaths (d - 2) st]
        pathF = [Seq (Assume $ Not g) sf' | sf' <- getPaths (d - 2) sf]
getPaths d (Seq s1 s2) =
  [ Seq s1' s2'
  | l1 <- [1..(d - 1)], let l2 = (d - 1) - l1
  , s1' <- getPaths l1 s1
  , s2' <- getPaths l2 s2
  ]
-- getPaths d w@(While _ _) =
--   concat [ buildLoops w unrolls d | unrolls <- [0..(previousEven d) / 2] ]
getPaths d stmt = [stmt | d == 1]

getPathPairs :: Int -> Stmt -> [[Stmt]]
getPathPairs d stmt = do
  l1 <- [1..d]
  let l2 = d - l1
  s1 <- getPaths l1 stmt
  s2 <- getPaths l2 stmt
  return [s1, s2]


replicateStmt :: Int -> Stmt -> Stmt
replicateStmt n stmt = sequenceStmts $ replicate n stmt

sequenceStmts :: [Stmt] -> Stmt
sequenceStmts = foldr1 Seq

buildLoops :: Stmt -> Int -> Int -> [Stmt]
buildLoops (While g _) 0 1 = [Assume $ Not g]
buildLoops (While g s) unrolls depth =
  [Assume g] ++ intersperse (Assume g) loops ++ [Assume $ Not g]
  where loops = []
buildLoops _ _ _ = []

previousEven :: Int -> Int
previousEven n = if odd n then n - 1 else n - 2
