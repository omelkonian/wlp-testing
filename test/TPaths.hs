module TPaths where

import Test.HUnit

import AST
import Paths
import TExamples (minind)

pathTests =
  [ pathsTest1
  , pathsTest2
  , pathsTest3
  , pathsTest4
  , pathsTest5
  , pathsTest6
  ]

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

pathsTest6 = getAllPaths 0 9 s @?= ps
  where
    s = minind
    ps = [ Assume (i .< _N) <:> VarStmt ["min"] (
             ["min", "r"] .:= ["a".!i, i]
             <:> Assume (Not $ i .< _N)
           ) <:> Assert (markGoal $ Forall ["j"] (i .<= j /\ j .< _N ==> "a".!r .<= "a".!j))
         , Assume (i .< _N) <:> VarStmt ["min"] (
             ["min", "r"] .:= ["a".!i, i]
             <:> Assume (i .< _N)
             <:> Assume ("a".!i .< min)
             <:> ["min", "r"] .:= ["a".!i, i]
             <:> ["i"] .:= [i .+ LitInt 1]
             <:> Assume (Not $ i .< _N)
           ) <:> Assert (markGoal $ Forall ["j"] (i .<= j /\ j .< _N ==> "a".!r .<= "a".!j))
         , Assume (i .< _N) <:> VarStmt ["min"] (
             ["min", "r"] .:= ["a".!i, i]
             <:> Assume (i .< _N)
             <:> Assume (Not $ "a".!i .< min)
             <:> Skip
             <:> ["i"] .:= [i .+ LitInt 1]
             <:> Assume (Not $ i .< _N)
           ) <:> Assert (markGoal $ Forall ["j"] (i .<= j /\ j .< _N ==> "a".!r .<= "a".!j))
         ]
    [i, _N, min, r, j] = map n ["i", "n", "min", "r", "j"]
