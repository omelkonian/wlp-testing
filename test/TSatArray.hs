module TSatArray where

import Test.HUnit
import Data.SBV (runSMT)
import qualified Data.Map as M
import System.IO.Unsafe

import AST
import SAT

satArrayTests =
  [ satTest1
  , satTest2
  , satTest3
  , satTest4
  , satTest5
  , satTest6
  , satTest7
  , satTest8
  , satTest9
  , satTest10
  , satTest11
  , satTest12
  , satTest13
  , satTest14
  , satTest15
  , satTest16
  , satTest17
  ]

testSat ass g = unsafePerformIO $ runSMT $ check ass g

satTest1 = testSat ass g @?= "Ignore"
  where
      ass = [ k .< _N
            , Not $ k .< _N
            ]
      g = Forall ["$0"] $ k .<= l /\ l .< _N ==> a_k .<= a_l
      [k, l, _N, r] = map n ["k", "$0", "N", "r"]
      [a_k, a_l] = ["a".!k, "a".!l]

satTest2 = testSat ass g @?= "Fail"
  where
      ass = [ k .< _N ]
      g = Forall ["$0"] $ k .<= l /\ l .< _N ==> a_r .<= a_l
      [k, l, _N, r] = map n ["k", "$0", "N", "r"]
      [a_r, a_l] = ["a".!r, "a".!l]

satTest3 = testSat ass g @?= "Ignore"
  where
      ass = [ k .< _N
            , a_k .< a_k
            , Not $ k .+ i 1 .< _N
            ]
      g = Forall ["$0"] $ k .<= l /\ l .< _N ==> a_r .<= a_l
      [k, l, _N, r] = map n ["k", "$0", "N", "r"]
      [a_r, a_l, a_k] = ["a".!r, "a".!l, "a".!k]

satTest4 = testSat ass g @?= "Pass"
  where
      ass = [ k .< _N
            , Not $ a_k .< a_k
            , Not $ k .+ i 1 .< _N
            ]
      g = Forall ["$0"] $ k .+ i 1 .<= l /\ l .< _N ==> a_k .<= a_l
      [k, l, _N, r] = map n ["k", "$0", "N", "r"]
      [a_l, a_k] = ["a".!l, "a".!k]

satTest5 = testSat ass g @?= "Pass"
  where
      ass = [ k .< _N
            , Not $ a_k .< a_k
            , k .+ i 1 .< _N
            , a_k' .< a_k
            , Not $ k .+ i 1 .+ i 1 .< _N
            ]
      g = Forall ["$0"] $
            k .+ i 1 .+ i 1 .<= l /\ l .< _N ==> a_k' .<= a_l
      [k, l, _N, r] = map n ["k", "$0", "N", "r"]
      [a_l, a_k, a_k'] = ["a".!l, "a".!k, "a".!(k .+ i 1)]

satTest6 = testSat ass g @?= "Pass"
  where
      ass = [ a_5 .= i 1 ]
      g = (i 5 .= i 5 ==> i 2 .= i 2) /\ (i 5 .= i 2 ==> a_5 .= i 2)
      a_5 = "a" .! i 5

satTest7 = testSat ass g @?= "Fail"
  where
      ass = [ a_5 .= i 1 ]
      g = a_1 .= i 1
      [a_5, a_1] = ["a" .! i 5, "a" .! i 1]

satTest8 = testSat [] ("a" .! i 0 .= i 0) @?= "Fail"

satTest9 = testSat ass g @?= "Pass"
  where
    ass = [a_0 .> i 0]
    g = a_0 .+ i 1 .> i 1
    a_0 = "a" .! i 0

satTest10 = testSat ass g @?= "Pass"
  where
    ass = ["a" .! i 0 .> i 0]
    g = ("a" .! (i 1 .- i 1)) .+ i 1 .> i 1

satTest11 = testSat ass g @?= "Fail"
  where
    ass = ["a" .! i 0 .> i 0]
    g = ("a" .! (i 1 .- i 1)) .> i 1

satTest12 = testSat ass g @?= "Pass"
  where
    ass = [a_0 .= i 0]
    g = Forall ["j"] $ j .= i 0 ==> a_j .= i 0
    j = n "j"
    [a_0, a_j] = ["a" .! i 0, "a" .! j]

satTest13 = testSat ass g @?= "Fail"
  where
    ass = []
    g = j .< i 2 ==> a_j .= i 0
    j = n "j"
    [a_0, a_j] = ["a" .! i 0, "a" .! j]

satTest14 = testSat ass g @?= "Pass"
  where
    ass = [a_0 .= i 0, a_1 .= i 0]
    g = Forall ["j"] $ j .= i 0 \/ j .= i 1 ==> a_j .= i 0
    j = n "j"
    [a_0, a_1, a_j] = ["a" .! i 0, "a" .! i 1, "a" .! j]

satTest15 = testSat [] g @?= "Fail"
  where
    g = Forall ["x"] $ x .= i 1 ==> a_x .= a_0
    [x, a_x, a_0] = [n "x", "a" .! x, "a" .! i 0]

satTest16 = testSat ass g @?= "Pass"
  where
    ass = [a_0 .= i 1, a_1 .= i 1]
    g = Forall ["x"] $ x .= i 0 \/ x .= i 1 ==> a_x .=  i 1
    [x, a_x, a_0, a_1] = [n "x", "a" .! x, "a" .! i 0, "a" .! i 1]

satTest17 = testSat ass g @?= "Fail"
  where
    ass = [a_0 .= i 1, a_1 .= i 1]
    g = Forall ["x"] $ x .= i 0 \/ x .= i 2 ==> a_x .=  i 1
    [x, a_x, a_0, a_1] = [n "x", "a" .! x, "a" .! i 0, "a" .! i 1]
