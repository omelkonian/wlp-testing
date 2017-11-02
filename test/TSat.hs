module TSat where

import Test.HUnit
import Data.SBV (runSMT)
import qualified Data.Map as M
import System.IO.Unsafe

import AST
import SAT

satTests =
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
  ]

testSat ass g = unsafePerformIO $ runSMT $ check ass g

satTest1 = testSat ass g @?= "Fail"
  where
    ass = [x .>= i (-1), Not $ x .> i 0]
    g = y .= i 0
    [x, y] = map n ["x", "y"]

satTest2 = testSat ass g @?= "Pass"
  where
    ass = [x .>= i 1, x .< i 2, x .= y .+ i 1 .- i 0]
    g = y .= i 0
    [x, y] = map n ["x", "y"]

satTest3 = testSat ass g @?= "Fail"
  where
    ass = [x .>= i 1, x .< i 2, x .= y .+ i 1 .- i 0]
    g = y .> i 0 \/ y .= i 1
    [x, y] = map n ["x", "y"]

satTest4 = testSat [] g @?= "Pass"
  where
    g = Forall ["x"] $ x .= i 1 ==> x .+ i 1 .= i 2
    x = n "x"

satTest5 = testSat [] g @?= "Fail"
  where
    g = Forall ["x"] $ x .= i 1 ==> x .+ i 1 .= i 1
    x = n "x"

satTest6 = testSat ass g @?= "Pass"
  where
    ass = [ x .= i 0 ]
    g = a_x .= a_0
    [x, a_x, a_0] = [n "x", "a" .! x, "a" .! i 0]

satTest7 = testSat ass g @?= "Fail"
  where
    ass = [ x .= i 0 ]
    g = a_x .= a_1
    [x, a_x, a_1] = [n "x", "a" .! x, "a" .! i 1]

satTest8 = testSat [] g @?= "Fail"
  where
    g = x .> i 1 ==> x .+ i 1 .= i 1
    x = n "x"

satTest9 = testSat [] g @?= "Fail"
  where
    g = Forall ["x"] $
          (x .< i 1) /\ (x .< i 1 ==> x .+ i 1 .= i 1)
    x = n "x"

satTest10 = testSat [] g @?= "Fail"
  where
    g = Forall ["x"] $ x .< i 2 ==> x .+ i 1 .= i 1
    x = n "x"

satTest11 = testSat [] g @?= "Fail"
  where
    g = Forall ["x"] $ x .= i 2 ==> x .+ i 1 .= i 1
    x = n "x"

satTest12 = testSat [] g @?= "Fail"
  where
    g = "a" .! x .= i 0
    x = n "x"
