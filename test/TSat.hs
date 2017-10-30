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
  , satTest17
  , satTest18
  , satTest19
  , satTest20
  , satTest21
  , satTest22
  , satTest23
  , satTest24
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

satGoal model goal = unsafePerformIO $ runSMT $ do
  smtVars <- genVars (getVars goal)
  checkGoal (model, smtVars, M.empty) goal

satTest5 = satGoal M.empty _T @?= True

satTest6 = satGoal model goal @?= True
  where
    model = M.fromList [("x", 10)]
    goal = x .> i 1
    x = n "x"

satTest7 = satGoal model goal @?= True
  where
    model = M.fromList [("x", 1), ("y", 2)]
    goal = x .>= i 1 ==> x .+ y .= i 3
    [x, y] = map n ["x", "y"]

satTest8 = satGoal model goal @?= True
  where
    model = M.fromList [("x", 1), ("y", 2)]
    goal = x .> i 1 ==> x .+ y .= i 3
    [x, y] = map n ["x", "y"]

satTest9 = satGoal model goal @?= False
  where
    model = M.fromList [("x", 1), ("y", 2)]
    goal = x .>= i 1 ==> x .+ y .= i 4
    [x, y] = map n ["x", "y"]

satTest10 = satGoal model goal @?= False
  where
    model = M.fromList [("x", 1), ("y", 2)]
    goal = x .= i 1 /\ y .= i 2 /\ x .+ y .= i 4
    [x, y] = map n ["x", "y"]

satTest11 = satGoal model goal @?= True
  where
    model = M.fromList [("x", 1), ("y", 2)]
    goal = x .= i 1 /\ y .= i 2 ==> _T /\ (_F \/ _T)
    [x, y] = map n ["x", "y"]

satTest12 = satGoal model goal @?= False
  where
    model = M.fromList [("x", 1), ("y", 2)]
    goal = x .= i 1 /\ y .= i 2 ==> _T /\ (_T ==> _F)
    [x, y] = map n ["x", "y"]

testSat ass g = unsafePerformIO $ runSMT $ check ass g

satTest13 = testSat ass g @?= "Fail"
  where
    ass = [x .>= i (-1), Not $ x .> i 0]
    g = y .= i 0
    [x, y] = map n ["x", "y"]

satTest14 = testSat ass g @?= "Pass"
  where
    ass = [x .>= i 1, x .< i 2, x .= y .+ i 1 .- i 0]
    g = y .= i 0
    [x, y] = map n ["x", "y"]

satTest15 = testSat ass g @?= "Fail"
  where
    ass = [x .>= i 1, x .< i 2, x .= y .+ i 1 .- i 0]
    g = y .> i 0 \/ y .= i 1
    [x, y] = map n ["x", "y"]

satTest16 = testSat [] g @?= "Pass"
  where
    g = Forall ["x"] $ x .= i 1 ==> x .+ i 1 .= i 2
    x = n "x"

satTest17 = testSat [] g @?= "Fail"
  where
    g = Forall ["x"] $ x .= i 1 ==> x .+ i 1 .= i 1
    x = n "x"

satTest18 = testSat ass g @?= "Pass"
  where
    ass = [ x .= i 0 ]
    g = a_x .= a_0
    [x, a_x, a_0] = [n "x", "a" .! x, "a" .! i 0]

satTest19 = testSat ass g @?= "Fail"
  where
    ass = [ x .= i 0 ]
    g = a_x .= a_1
    [x, a_x, a_1] = [n "x", "a" .! x, "a" .! i 1]

satTest20 = testSat [] g @?= "Fail"
  where
    g = x .> i 1 ==> x .+ i 1 .= i 1
    x = n "x"

satTest21 = testSat [] g @?= "Fail"
  where
    g = Forall ["x"] $
          (x .< i 1) /\ (x .< i 1 ==> x .+ i 1 .= i 1)
    x = n "x"

satTest22 = testSat [] g @?= "Fail"
  where
    g = Forall ["x"] $ x .< i 2 ==> x .+ i 1 .= i 1
    x = n "x"

satTest23 = testSat [] g @?= "Fail"
  where
    g = Forall ["x"] $ x .= i 2 ==> x .+ i 1 .= i 1
    x = n "x"

satTest24 = testSat [] g @?= "Fail"
  where
    g = "a" .! x .= i 0
    x = n "x"
