module TWlp where

import Test.HUnit

import AST
import Paths
import Wlp
import Normalizer
import TExamples

wlpTests =
  [ wlpTest1
  , wlpTest2
  , wlpTest3
  ]

wlpTest1 = map (stripMarks . (`wlp` _T)) (getAllPaths 0 9 prog) @?= wlps
  where
    prog = example
    wlps = [ x .>= i (-1) ==>
                Not (x .> i 0) ==>
                  y .= i 0 /\ _T
           , x .>= i (-1) ==>
                x .> i 0 ==>
                  Not (x .+ i 1 .> i 0) ==>
                    x .+ i 1 .= i 0 /\ _T
           , x .>= i (-1) ==>
                x .> i 0 ==>
                  x .+ i 1 .> i 0 ==>
                    Not (x .+ i 1 .+ i 1 .> i 0) ==>
                      x .+ i 1 .+ i 1 .= i 0 /\ _T
           ]
    [x, y] = map n ["x", "y"]

wlpTest2 = map (stripMarks . (`wlp` _T)) (getAllPaths 0 9 prog) @?= wlps
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

wlpTest3 = stripMarks (head (getAllPaths 0 9 prog) `wlp` _T) @?= wlp'
  where
    prog = swap
    wlp' =
      x .= a_i /\ y .= a_j ==>
        x .= Cond (j .= j) a_i (Cond (j .= i) a_j a_j)
          /\
        y .= Cond (i .= j) a_i (Cond (i .= i) a_j a_i)
    [x, y, a, i, j, tmp] = map n ["x", "y", "a", "i", "j", "tmp"]
    [a_i, a_j] = ["a".!i, "a".!j]
