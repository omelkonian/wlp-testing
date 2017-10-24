module Paths where

import AST

getAllPaths :: Int -> Int -> Program -> [Stmt]
getAllPaths depthStart depthEnd prog =
  concat [getPaths d (body prog) | d <- [depthStart..depthEnd]]

getPaths :: Int -> Stmt -> [Stmt]
getPaths d (VarStmt ids s) =
  [VarStmt ids s' | s' <- getPaths (d - 1) s]

getPaths d (Ite g st sf) = pathT ++ pathF
  where pathT = [Seq (Assume g) st' | st' <- getPaths (d - 1) st]
        pathF = [Seq (Assume $ Not g) sf' | sf' <- getPaths (d - 1) sf]

getPaths d (Seq s1 s2) =
  [Seq s1' s2' | [s1', s2'] <- getMultiPaths d [s1, s2]]

getPaths 1 (While inv g _) = [assume inv (Not g)]
getPaths d (While inv g body) =
  [ foldr1 Seq $ insertConds $ concat paths
  | unrolls <- [1..quot d 2]
  , let paths = getMultiPaths (d - 2) (replicate unrolls body)
  , not $ null paths
  , let insertConds = foldr (\s -> (++) [assume inv g, s]) [assume inv (Not g)]
  ]

getPaths d stmt = [stmt | d == 1]

assume Nothing q = Assume q
assume (Just p) q = Assume $ and_ p q

getMultiPaths :: Int -> [Stmt] -> [[Stmt]]
getMultiPaths d stmts =
  map (map fst) (getMultiPaths' d stmts)
getMultiPaths' _ [] = [[]]
getMultiPaths' depth (stmt : stmts) =
  [ (l, d) : rest
  | d <- [1..depth]
  , l <- getPaths d stmt
  , rest <- getMultiPaths' (depth - d) stmts
  , sum (d : map snd rest) == depth
  ]
