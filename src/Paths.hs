module Paths (getAllPaths) where

import AST

-- | Get all possible paths of a given statement.
getAllPaths :: Int -> Int -> Stmt -> [Stmt]
getAllPaths depthStart depthEnd prog =
  concat [getPaths d prog | d <- [depthStart..depthEnd]]

-- | Get all possible paths of a given statement with a certain depth.
getPaths :: Int -> Stmt -> [Stmt]
getPaths d (VarStmt ids s) =
  [VarStmt ids s' | s' <- getPaths (d - 1) s]
getPaths d (Ite g st sf) = pathT ++ pathF
  where pathT = [Assume g <:> st' | st' <- getPaths (d - 1) st]
        pathF = [(Assume $ Not g) <:> sf' | sf' <- getPaths (d - 1) sf]
getPaths d (Seq s1 s2) =
  [s1' <:> s2' | [s1', s2'] <- getMultiPaths d [s1, s2]]
getPaths 1 (While inv g _) =
  [assume inv (Not g)]
getPaths d (While inv g body) =
  [ foldr1 (<:>) $ insertConds paths
  | unrolls <- [1..(if even d then quot d 2 - 1 else quot d 2)]
  , paths <- getMultiPaths (d - unrolls - 1) (replicate unrolls body)
  , not $ null paths
  , let continue = Assume g
  , let break = Assume $ Not g
  , let insertConds = foldr (\s -> (++) (continue : seqsToList s)) [break]
  ]
getPaths d stmt = [stmt | d == 1]

-- | Get all possible path combinations of the given statements with a certain
-- total depth.
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

-- | Convert sequenced statements into a list of statements.
seqsToList :: Stmt -> [Stmt]
seqsToList (Seq s s') = seqsToList s ++ seqsToList s'
seqsToList s = [s]

assume Nothing q = Assume q
assume (Just p) q = Assume $ p /\ q
