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
  , satTest13
  , satTest14
  , satTest15
  , satTest16
  ]

satAss ass = unsafePerformIO $ runSMT $ do
  smtVars <- genVars (getManyVars ass)
  checkAssumptions (smtVars, M.empty) ass

satTest1 = satAss assumptions @?= Just res
  where
    assumptions = [x .> i 1]
    res = M.fromList [("x", 2)]
    x = n "x"

satTest2 = satAss [] @?= Just M.empty

satTest3 = satAss assumptions @?= Nothing
  where
    assumptions = [x .> i 0 /\ x .< i 0]
    x = n "x"

satTest4 = satAss assumptions @?= Just res
  where
    assumptions = [x .> i 1 /\ x .< i 10 /\ x .+ x .= i 10]
    res = M.fromList [("x", 5)]
    x = n "x"

testSat ass g = unsafePerformIO $ runSMT $ check ass g

satTest5 = testSat ass g @?= "Fail"
  where
    ass = [x .>= i (-1), Not $ x .> i 0]
    g = y .= i 0
    [x, y] = map n ["x", "y"]

satTest6 = testSat ass g @?= "Pass"
  where
    ass = [x .>= i 1, x .< i 2, x .= y .+ i 1 .- i 0]
    g = y .= i 0
    [x, y] = map n ["x", "y"]

satTest7 = testSat ass g @?= "Fail"
  where
    ass = [x .>= i 1, x .< i 2, x .= y .+ i 1 .- i 0]
    g = y .> i 0 \/ y .= i 1
    [x, y] = map n ["x", "y"]

satTest8 = testSat [] g @?= "Pass"
  where
    g = Forall ["x"] $ x .= i 1 ==> x .+ i 1 .= i 2
    x = n "x"

satTest9 = testSat [] g @?= "Fail"
  where
    g = Forall ["x"] $ x .= i 1 ==> x .+ i 1 .= i 1
    x = n "x"

satTest10 = testSat ass g @?= "Pass"
  where
    ass = [ x .= i 0 ]
    g = a_x .= a_0
    [x, a_x, a_0] = [n "x", "a" .! x, "a" .! i 0]

satTest11 = testSat ass g @?= "Fail"
  where
    ass = [ x .= i 0 ]
    g = a_x .= a_1
    [x, a_x, a_1] = [n "x", "a" .! x, "a" .! i 1]

satTest12 = testSat [] g @?= "Fail"
  where
    g = x .> i 1 ==> x .+ i 1 .= i 1
    x = n "x"

satTest13 = testSat [] g @?= "Fail"
  where
    g = Forall ["x"] $
          (x .< i 1) /\ (x .< i 1 ==> x .+ i 1 .= i 1)
    x = n "x"

satTest14 = testSat [] g @?= "Fail"
  where
    g = Forall ["x"] $ x .< i 2 ==> x .+ i 1 .= i 1
    x = n "x"

satTest15 = testSat [] g @?= "Fail"
  where
    g = Forall ["x"] $ x .= i 2 ==> x .+ i 1 .= i 1
    x = n "x"

satTest16 = testSat [] g @?= "Fail"
  where
    g = "a" .! x .= i 0
    x = n "x"
