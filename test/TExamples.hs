module TExamples where

import AST

example =
  Assume (x .>= i (-1))
  <:> While Nothing (x .> i 0) (
        ["x"] .:= [x .+ i 1]
        <:> ["y"] .:= [x]
      )
  <:> Assert (markGoal $ y .= i 0)
  where [x, y] = map n ["x", "y"]

example2 =
  Assume (x .>= i 0)
  <:> While Nothing (x .> i 0) (
        ["x"] .:= [x .- i 1]
      )
      <:> ["y"] .:= [x]
  <:> Assert (markGoal $ y .= i 0)
  where [x, y] = map n ["x", "y"]

swap =
  Assume (a .= a0)
  <:> VarStmt ["tmp"] (
        ["tmp"] .:= [a_i]
        <:> ["a"] .:= [RepBy a i a_j]
        <:> ["a"] .:= [RepBy a j tmp]
        <:> ["a'"] .:= [a]
      )
  <:> Assert (markGoal $ a'_i .= a0_j /\ a'_j .= a0_i)
  where
    [a, a0, i, j, tmp] = map n ["a", "a0", "i", "j", "tmp"]
    [a_i, a_j, a0_i, a0_j, a'_i, a'_j] =
      ["a".!i, "a".!j, "a0".!i, "a0".!j, "a'".!i, "a'".!j]

minind =
  Assume (i .< _N) <:> VarStmt ["min"] (
    ["min", "r"] .:= ["a".!i, i]
    <:> While Nothing (i .< _N) (
      Ite ("a".!i .< min) (["min", "r"] .:= ["a".!i, i]) Skip
      <:> ["i"] .:= [i .+ LitInt 1]
    )
  ) <:> Assert (markGoal $ Forall ["j"] (i .<= j /\ j .< _N ==> "a".!r .<= "a".!j))
  where
    [i, _N, min, r, j] = map n ["i", "n", "min", "r", "j"]

loopInvariant =
  While (Just $ y .= i 0) (x .> i 0) (
    ["x"] .:= [x .- i 1]
  )
  <:> Assert (markGoal $ y .= i 0 /\ x .<= i 0)
  where
    [x, y] = map n ["x", "y"]

minind2 =
  Assume (i .< _N) <:> VarStmt ["min"] (
    ["min", "r"] .:= ["a".!i, i]
    <:> While Nothing (i .< _N) (
      Ite ("a".!i .> min) (["min", "r"] .:= ["a".!i, i]) Skip
      <:> ["i"] .:= [i .+ LitInt 1]
    )
  ) <:> Assert (markGoal $ Forall ["j"] (i .<= j /\ j .< _N ==> "a".!r .<= "a".!j))
  where
    [i, _N, min, r, j] = map n ["i", "n", "min", "r", "j"]
