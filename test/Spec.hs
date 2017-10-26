import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Monad.State (evalState)

import AST
import Renaming
import Paths
import Wlp
import Normalizer


main = defaultMain tests

tests =
  [ testCase "RENAMING" renamingTest
  , testGroup "PATHS"
    [ testCase "PATHS_1" pathsTest1
    , testCase "PATHS_2" pathsTest2
    , testCase "PATHS_3" pathsTest3
    , testCase "PATHS_4" pathsTest4
    , testCase "PATHS_5" pathsTest5
    , testCase "PATHS_6" pathsTest6
    ]
  , testGroup "WLP"
    [ testCase "WLP_1" wlpTest1
    , testCase "WLP_2" wlpTest2
    , testCase "WLP_3" wlpTest3
    ]
  , testCase "NORM_1" normTest1
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

pathsTest1 = getAllPaths 0 10 s @?= ps
  where
    s = Ite g Skip (Skip <:> Skip)
    ps = [ Assume g <:> Skip
         , Assume ng <:> Skip <:> Skip
         ]
    [g, ng] = [n "g", Not $ n "g"]

pathsTest2 = getAllPaths 0 17 s @?= ps
  where
    s = While Nothing g Skip
    ps = [ Assume ng
         , Assume g <:> Skip
         <:> Assume ng
         , Assume g <:> Skip <:> Assume g <:> Skip
         <:> Assume ng
         , Assume g <:> Skip <:> Assume g <:> Skip  <:> Assume g <:> Skip
         <:> Assume ng
         , Assume g <:> Skip <:> Assume g <:> Skip <:> Assume g <:> Skip
         <:> Assume g <:> Skip
         <:> Assume ng
         , Assume g <:> Skip <:> Assume g <:> Skip <:> Assume g <:> Skip
         <:> Assume g <:> Skip <:> Assume g <:> Skip
         <:> Assume ng
         , Assume g <:> Skip <:> Assume g <:> Skip <:> Assume g <:> Skip
         <:> Assume g <:> Skip <:> Assume g <:> Skip <:> Assume g <:> Skip
         <:> Assume ng
         , Assume g <:> Skip <:> Assume g <:> Skip <:> Assume g <:> Skip
         <:> Assume g <:> Skip <:> Assume g <:> Skip <:> Assume g <:> Skip
         <:> Assume g <:> Skip
         <:> Assume ng
         , Assume g <:> Skip <:> Assume g <:> Skip <:> Assume g <:> Skip
         <:> Assume g <:> Skip <:> Assume g <:> Skip <:> Assume g <:> Skip
         <:> Assume g <:> Skip <:> Assume g <:> Skip
         <:> Assume ng
         ]
    [g, ng] = [n "g", Not $ n "g"]

pathsTest3 = getAllPaths 0 22 s @?= ps
  where
    s = While Nothing g (Skip <:> Skip)
    ps = [ Assume ng
         , Assume g <:> Skip <:> Skip
         <:> Assume ng
         , Assume g <:> Skip <:> Skip <:> Assume g <:> Skip <:> Skip
         <:> Assume ng
         , Assume g <:> Skip <:> Skip <:> Assume g <:> Skip <:> Skip
         <:> Assume g <:> Skip <:> Skip
         <:> Assume ng
         , Assume g <:> Skip <:> Skip <:> Assume g <:> Skip <:> Skip
         <:> Assume g <:> Skip <:> Skip <:> Assume g <:> Skip <:> Skip
         <:> Assume ng
         , Assume g <:> Skip <:> Skip <:> Assume g <:> Skip <:> Skip
         <:> Assume g <:> Skip <:> Skip <:> Assume g <:> Skip <:> Skip
         <:> Assume g <:> Skip <:> Skip
         <:> Assume ng
         , Assume g <:> Skip <:> Skip <:> Assume g <:> Skip <:> Skip
         <:> Assume g <:> Skip <:> Skip <:> Assume g <:> Skip <:> Skip
         <:> Assume g <:> Skip <:> Skip <:> Assume g <:> Skip <:> Skip
         <:> Assume ng
         , Assume g <:> Skip <:> Skip <:> Assume g <:> Skip <:> Skip
         <:> Assume g <:> Skip <:> Skip <:> Assume g <:> Skip <:> Skip
         <:> Assume g <:> Skip <:> Skip <:> Assume g <:> Skip <:> Skip
         <:> Assume g <:> Skip <:> Skip
         <:> Assume ng
         ]
    [g, ng] = [n "g", Not $ n "g"]

pathsTest4 = getAllPaths 0 11 s @?= ps
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
         , Assume a
         <:> Assume b <:> Skip
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
         <:> Assume b <:> Skip
         <:> Assume (Not b)
         , Assume (Not a)
         <:> Assume c <:> Skip <:> Skip
         <:> Assume c <:> Skip <:> Skip
         <:> Assume c <:> Skip <:> Skip
         <:> Assume (Not c)
         ]
    [a, b, c] = map n ["a", "b", "c"]

pathsTest5 = getAllPaths 0 9 s @?= ps
  where
    s = While Nothing (i .< _N) (
          Ite ("a".!i .< min) (["min", "r"] .:= ["a".!i, i]) Skip
        <:> ["i"] .:= [i .+ LitInt 1]
        )
    ps = [ Assume (Not $ i .< _N)
         , Assume (i .< _N)
         <:> Assume ("a".!i .< min)
         <:> ["min", "r"] .:= ["a".!i, i]
         <:> ["i"] .:= [i .+ LitInt 1]
         <:> Assume (Not $ i .< _N)
         , Assume (i .< _N)
         <:> Assume (Not $ "a".!i .< min)
         <:> Skip
         <:> ["i"] .:= [i .+ LitInt 1]
         <:> Assume (Not $ i .< _N)
         , Assume (i .< _N)
         <:> Assume ("a".!i .< min)
         <:> ["min", "r"] .:= ["a".!i, i]
         <:> ["i"] .:= [i .+ LitInt 1]
         <:> Assume (i .< _N)
         <:> Assume ("a".!i .< min)
         <:> ["min", "r"] .:= ["a".!i, i]
         <:> ["i"] .:= [i .+ LitInt 1]
         <:> Assume (Not $ i .< _N)
         , Assume (i .< _N)
         <:> Assume ("a".!i .< min)
         <:> ["min", "r"] .:= ["a".!i, i]
         <:> ["i"] .:= [i .+ LitInt 1]
         <:> Assume (i .< _N)
         <:> Assume (Not $ "a".!i .< min)
         <:> Skip
         <:> ["i"] .:= [i .+ LitInt 1]
         <:> Assume (Not $ i .< _N)
         , Assume (i .< _N)
         <:> Assume (Not $ "a".!i .< min)
         <:> Skip
         <:> ["i"] .:= [i .+ LitInt 1]
         <:> Assume (i .< _N)
         <:> Assume ("a".!i .< min)
         <:> ["min", "r"] .:= ["a".!i, i]
         <:> ["i"] .:= [i .+ LitInt 1]
         <:> Assume (Not $ i .< _N)
         , Assume (i .< _N)
         <:> Assume (Not $ "a".!i .< min)
         <:> Skip
         <:> ["i"] .:= [i .+ LitInt 1]
         <:> Assume (i .< _N)
         <:> Assume (Not $ "a".!i .< min)
         <:> Skip
         <:> ["i"] .:= [i .+ LitInt 1]
         <:> Assume (Not $ i .< _N)
         ]
    [i, _N, min, r] = map n ["i", "n", "min", "r"]

minind =
  Assume (i .< _N) <:> VarStmt ["min"] (
    ["min", "r"] .:= ["a".!i, i]
    <:> While Nothing (i .< _N) (
      Ite ("a".!i .< min) (["min", "r"] .:= ["a".!i, i]) Skip
      <:> ["i"] .:= [i .+ LitInt 1]
    )
  ) <:> Assert (Forall ["j"] (i .<= j /\ j .< _N ==> "a".!r .<= "a".!j))
  where
    [i, _N, min, r, j] = map n ["i", "n", "min", "r", "j"]

pathsTest6 = getAllPaths 0 9 s @?= ps
  where
    s = minind
    ps = [ Assume (i .< _N) <:> VarStmt ["min"] (
             ["min", "r"] .:= ["a".!i, i]
             <:> Assume (Not $ i .< _N)
           ) <:> Assert (Forall ["j"] (i .<= j /\ j .< _N ==> "a".!r .<= "a".!j))
         , Assume (i .< _N) <:> VarStmt ["min"] (
             ["min", "r"] .:= ["a".!i, i]
             <:> Assume (i .< _N)
             <:> Assume ("a".!i .< min)
             <:> ["min", "r"] .:= ["a".!i, i]
             <:> ["i"] .:= [i .+ LitInt 1]
             <:> Assume (Not $ i .< _N)
           ) <:> Assert (Forall ["j"] (i .<= j /\ j .< _N ==> "a".!r .<= "a".!j))
         , Assume (i .< _N) <:> VarStmt ["min"] (
             ["min", "r"] .:= ["a".!i, i]
             <:> Assume (i .< _N)
             <:> Assume (Not $ "a".!i .< min)
             <:> Skip
             <:> ["i"] .:= [i .+ LitInt 1]
             <:> Assume (Not $ i .< _N)
           ) <:> Assert (Forall ["j"] (i .<= j /\ j .< _N ==> "a".!r .<= "a".!j))
         ]
    [i, _N, min, r, j] = map n ["i", "n", "min", "r", "j"]

wlpTest1 = map (`wlp` _T) (getAllPaths 0 9 prog) @?= wlps
  where
    prog = Assume (x .>= i 1)
       <:> While Nothing (x .> i 0)
           (["x"] .:= [x .+ i 1] <:> ["y"] .:= [x])
       <:> Assert (y .= i 0)
    wlps = [ (x .>= i 1) ==>
                Not (x .> i 0) ==>
                  y .= i 0 /\ _T
           , (x .>= i 1) ==>
                (x .> i 0) ==>
                  Not (x .+ i 1 .> i 0) ==>
                    x .+ i 1 .= i 0 /\ _T
           , (x .>= i 1) ==>
                (x .> i 0) ==>
                  (x .+ i 1 .> i 0) ==>
                    Not (x .+ i 1 .+ i 1 .> i 0) ==>
                      x .+ i 1 .+ i 1 .= i 0 /\ _T
           ]
    [x, y] = map n ["x", "y"]

wlpTest2 = map (`wlp` _T) (getAllPaths 0 9 prog) @?= wlps
  where
    prog = minind
    wlps = [ i .< _N ==>
              Not (i .< _N) ==>
                Forall ["j"] (i .<= j /\ j .< _N ==> "a".!i .<= "a".!j)
           , i .< _N ==>
               i .< _N ==>
                "a".!i .< "a".!i ==>
                  Not (i .+ LitInt 1 .< _N) ==>
                    Forall ["j"] (i .+ LitInt 1 .<= j /\ j .< _N ==> "a".!i .<= "a".!j)
           , i .< _N ==>
               i .< _N ==>
                Not ("a".!i .< "a".!i) ==>
                  Not (i .+ LitInt 1 .< _N) ==>
                    Forall ["j"] (i .+ LitInt 1 .<= j /\ j .< _N ==> "a".!i .<= "a".!j)
           ]
    [i, _N, min, r, j] = map n ["i", "n", "min", "r", "j"]

wlpTest3 = head (getAllPaths 0 9 prog) `wlp` _T @?= wlp'
  where
    prog =
      Assume (a .= a0)
      <:> VarStmt ["tmp"] (
        ["tmp"] .:= [a_i]
        <:> ["a"] .:= [RepBy a i a_j]
        <:> ["a"] .:= [RepBy a j tmp]
        <:> ["a'"] .:= [a]
      )
      <:> Assert (a'_i .= a0_j /\ a'_j .= a0_i)
    wlp' =
      a .= a0 ==>
        Cond (i .= j) a_i (Cond (i .= i) a_j a_i) .= a0_j
          /\
        Cond (j .= j) a_i (Cond (j .= i) a_j a_j) .= a0_i
    [a, a0, i, j, a', tmp] = map n ["a", "a0", "i", "j", "a'", "tmp"]
    [a_i, a_j, a0_i, a0_j, a'_i, a'_j] =
      ["a".!i, "a".!j, "a0".!i, "a0".!j, "a'".!i, "a'".!j]

normTest1 = splitAssumptions e @?= (assumptions, g)
  where
    e = a1 ==> a2 ==> a3 ==> a4 ==> g
    assumptions = [a1, a2, a3, a4]
    a1 = x .> i 0
    a2 = Not $ x .>= i 0
    a3 = Forall ["k"] (k .> i 0 /\ k .> i 1)
    a4 = a_k .= i 0 \/ a_l .= i 0
    g = a_k .= i 0 /\ a_l .= i 0 ==> Not _T .= _F \/ _F .!= _F
    [x, k, l] = map n ["x", "k", "l"]
    [a_k, a_l] = ["a".!k, "a".!l]
