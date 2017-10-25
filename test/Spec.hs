import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Monad.State (evalState)

import AST
import Renaming
import Paths
import Wlp


main = defaultMain tests

tests =
  [ testCase "RENAMING" renamingTest
  , testCase "PATHS" pathsTest
  -- , testCase "WLP" wlpTest
  ]

renamingTest = evalState (rename prog1) 0 @?= prog2
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

pathsTest = getAllPaths 0 11 s @?= ps
  where
    s = Ite a
          (While Nothing b Skip)
          (While Nothing c (Skip <:> Skip))
    ps = [ Assume a <:> Assume (Not b)
         , Assume (Not a) <:> Assume (Not c)
         , Assume a
         <:> Assume b <:> Skip
         <:> Assume (Not b)
         , Assume (Not a)
         <:> Assume c <:> Skip <:> Skip
         <:> Assume (Not c)
         , Assume a
         <:> Assume b <:> Skip
         <:> Assume b <:> Skip
         <:> Assume (Not b)
         , Assume (Not a)
         <:> Assume c <:> Skip <:> Skip
         <:> Assume c <:> Skip <:> Skip
         <:> Assume (Not c)
         , Assume a
         <:> Assume b <:> Skip
         <:> Assume b <:> Skip
         <:> Assume b <:> Skip
         <:> Assume (Not b)
         , Assume a
         <:> Assume b <:> Skip
         <:> Assume b <:> Skip
         <:> Assume b <:> Skip
         <:> Assume b <:> Skip
         <:> Assume (Not b)
         , Assume (Not a)
         <:> Assume c <:> Skip <:> Skip
         <:> Assume c <:> Skip <:> Skip
         <:> Assume c <:> Skip <:> Skip
         <:> Assume (Not c)
         ]
    [a, b, c] = map n ["a", "b", "c"]


wlpTest = map (`wlp` _T) (getAllPaths 0 9 prog) @?= wlps
  where
    prog = Assume (x .>= i 1)
       <:> While Nothing (x .> i 0)
           (["x"] .:= [x .+ i 1] <:> ["y"] .:= [x])
       <:> Assert (y .= i 0)
    wlps = [ (x .>= i 1) ==>
                Not (x .> i 0) ==>
                  x .= i 0 ==>
                    _T
           , (x .>= i 1) ==>
                (x .> i 0) ==>
                  Not (x .+ i 1 .> i 0) ==>
                    x .+ i 1 .= i 0 ==>
                      _T
           , (x .>= i 1) ==>
                (x .> i 0) ==>
                  (x .+ i 1 .> i 0) ==>
                    Not (x .+ i 1 .+ i 1 .> i 0) ==>
                      x .+ i 1 .+ i 1 .= i 0 ==>
                        _T
           ]
    [x, y] = map n ["x", "y"]
