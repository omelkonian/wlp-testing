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
  ]

satAss vs ass = unsafePerformIO $ runSMT $ checkAssumptions vs ass
satTest1 = satAss vs assumptions @?= Just res
  where
    vs = ["x"]
    assumptions = [x .> i 1]
    res = M.fromList [("x", 2)]
    x = n "x"

satTest2 = satAss vs assumptions @?= Just res
  where
    vs = ["x"]
    assumptions = []
    res = M.fromList [("x", 0)]
    x = n "x"

satTest3 = satAss vs assumptions @?= Nothing
  where
    vs = ["x"]
    assumptions = [x .> i 0 /\ x .< i 0]
    x = n "x"

satTest4 = satAss vs assumptions @?= Just res
  where
    vs = ["x"]
    assumptions = [x .> i 1 /\ x .< i 10 /\ x .+ x .= i 10]
    res = M.fromList [("x", 5)]
    x = n "x"

satGoal model goal = unsafePerformIO $ runSMT $ checkGoal model goal
satTest5 = satGoal model goal @?= True
  where
    model = M.empty
    goal = _T

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

testSat vs ass g = unsafePerformIO $ runSMT $ check vs ass g
satTest13 = testSat vs ass g @?= "Fail"
  where
      vs = ["x"]
      ass = [x .>= i (-1), Not $ x .> i 0]
      g = y .= i 0
      [x, y] = map n ["x", "y"]

satTest14 = testSat vs ass g @?= "Pass"
  where
      vs = ["x", "y"]
      ass = [x .>= i 1, x .< i 2, x .= y .+ i 1 .- i 0]
      g = y .= i 0
      [x, y] = map n ["x", "y"]

satTest15 = testSat vs ass g @?= "Fail"
  where
      vs = ["x", "y"]
      ass = [x .>= i 1, x .< i 2, x .= y .+ i 1 .- i 0]
      g = y .> i 0 \/ y .= i 1
      [x, y] = map n ["x", "y"]

satTest16 = testSat vs ass g @?= "Ignore"
  where
      vs = ["k", "$0", "N"]
      ass = [ k .< _N
            , Not $ k .< _N
            ]
      g = Forall ["$0"] $ k .<= l /\ l .< _N ==> a_k .<= a_l
      [k, l, _N, r] = map n ["k", "$0", "N", "r"]
      [a_k, a_l] = ["a".!k, "a".!l]

satTest17 = testSat vs ass g @?= "Fail"
  where
      vs = ["k", "$0", "N"]
      ass = [ k .< _N ]
      g = Forall ["$0"] $ k .<= l /\ l .< _N ==> a_r .<= a_l
      [k, l, _N, r] = map n ["k", "$0", "N", "r"]
      [a_r, a_l] = ["a".!r, "a".!l]

satTest18 = testSat vs ass g @?= "Ignore"
  where
      vs = ["k", "$0", "N"]
      ass = [ k .< _N
            , a_k .< a_k
            , Not $ k .+ i 1 .< _N
            ]
      g = Forall ["$0"] $ k .<= l /\ l .< _N ==> a_r .<= a_l
      [k, l, _N, r] = map n ["k", "$0", "N", "r"]
      [a_r, a_l, a_k] = ["a".!r, "a".!l, "a".!k]

satTest19 = testSat vs ass g @?= "Pass"
  where
      vs = ["k", "$0", "N"]
      ass = [ k .< _N
            , Not $ a_k .< a_k
            , Not $ k .+ i 1 .< _N
            ]
      g = Forall ["$0"] $ k .+ i 1 .<= l /\ l .< _N ==> a_k .<= a_l
      [k, l, _N, r] = map n ["k", "$0", "N", "r"]
      [a_l, a_k] = ["a".!l, "a".!k]

satTest20 = testSat vs ass g @?= "Pass"
  where
      vs = ["k", "$0", "N"]
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
