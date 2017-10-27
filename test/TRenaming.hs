module TRenaming where

import Test.HUnit
import Control.Monad.State (evalState)

import AST
import Renaming

renamingTests =
  [ renamingTest1
  , renamingTest2
  , renamingTest3
  ]

renamingTest1 = evalState (rename prog1) 0 @?= prog2
  where prog1 = Assume (x .>= i 0)
            <:> ["x"] .:= [i (-1)]
            <:> VarStmt ["x", "y"]
                (   ["x", "y"] .:= [i 1, i 0]
                <:> VarStmt ["x"] (["x"] .:= [i 2] <:> ["y"] .:= [x])
                <:> ["y", "z"] .:= [x .- i 1, y .- i 2]
                )
            <:> Assert (x .<= i 0)
        prog2 = Assume (x .>= i 0)
            <:> ["x"] .:= [i (-1)]
            <:> VarStmt ["$1", "$2"]
                (   ["$1", "$2"] .:= [i 1, i 0]
                <:> VarStmt ["$0"] (["$0"] .:= [i 2] <:> ["$2"] .:= [t0])
                <:> ["$2", "z"] .:= [t1 .- i 1, t2 .- i 2]
                )
            <:> Assert (x .<= i 0)
        [x, y, t0, t1, t2] = map n ["x", "y", "$0", "$1", "$2"]

renamingTest2 = evalState (rename prog1) 0 @?= prog2
  where prog1 = ["x"] .:= [i 0]
                <:> VarStmt ["x"] (
                      ["x"] .:= [i 0]
                      <:> VarStmt ["x"] (
                        ["x"] .:= [i 0]
                        <:> Assert (x .= i 0)
                      )
                      <:> Assert (x .= i 0)
                    )
                <:> Assert (Forall ["x"] $ x .= i 0)
        prog2 = ["x"] .:= [i 0]
                <:> VarStmt ["$1"] (
                      ["$1"] .:= [i 0]
                      <:> VarStmt ["$0"] (
                        ["$0"] .:= [i 0]
                        <:> Assert (t0 .= i 0)
                      )
                      <:> Assert (t1 .= i 0)
                    )
                <:> Assert (Forall ["$2"] $ t2 .= i 0)
        [x, t0, t1, t2] = map n ["x", "$0", "$1", "$2"]

renamingTest3 = evalState (rename prog1) 0 @?= prog2
  where prog1 = ["x"] .:= [i 0]
                <:> VarStmt ["x"] (
                      ["x"] .:= [i 0]
                      <:> Assert (x .= i 0)
                    )
                <:> Assert (markGoal $ Forall ["x"] $ x .= i 0)
        prog2 = ["x"] .:= [i 0]
                <:> VarStmt ["$0"] (
                      ["$0"] .:= [i 0]
                      <:> Assert (t0 .= i 0)
                    )
                <:> Assert (markGoal $ Forall ["$1"] $ t1 .= i 0)
        [x, t0, t1] = map n ["x", "$0", "$1"]
