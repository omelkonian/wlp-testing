module TNorm where

import Test.HUnit

import AST
import Wlp
import Normalizer

normTests =
  [ normTest1
  , normTest2
  , normTest3
  , normTest4
  ]

normTest1 = normalize e @?= ([a], Just g)
  where
    e = markAssumption a ==> markGoal g
    a = n "x" .> i 0
    g = _T

normTest2 = normalize e @?= ([a], Just g)
  where
    e = markAssumption a ==> markGoal g
    a = x .> i 0 \/ x .< i 0
    g = y .> i 0 \/ y .< i 0
    [x, y] = map n ["x", "y"]

normTest3 = normalize e @?= ([a1, a2, a3], Just g)
  where
    e = markAssumption a1 ==> markAssumption a2 ==> markAssumption a3 ==> markGoal g
    a1 = Not $ x .>= i 0
    a2 = Forall ["k"] (k .> i 0 /\ k .> i 1)
    a3 = a_k .= i 0 \/ a_l .= i 0
    g = a_k .= i 0 /\ a_l .= i 0 ==> Not _T .= _F \/ _F .!= _F
    [x, k, l] = map n ["x", "k", "l"]
    [a_k, a_l] = ["a".!k, "a".!l]

normTest4 = prenexFixpoint e @?= e'
  where
    e = _T \/ Exist ["x"] _F ==> Forall ["y"] _T
    e' = Forall ["y", "x"] $ _T \/ _F ==> _T
