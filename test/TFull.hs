module TFull where

import Test.HUnit
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.State (evalState, forM)
import Data.SBV (runSMT)
import Data.SBV.Control (io)
import Data.Maybe (fromMaybe)
import Data.List (nub)

import AST
import Paths
import Renaming
import Wlp
import Normalizer
import SAT
import TExamples

import Debug.Trace
import PrettyPrinter (pp, ln)

fullTests =
  [ fullTest1
  , fullTest2
  ]

runExample n ex =
  unsafePerformIO $ runSMT $
    forM (take n $ getAllPaths 0 50 ex) (\path -> do
      let renamed = evalState (rename path) 0
      let predicate = wlp renamed _T
      let replaced = fixpointReplaceConds predicate
      let (assumptions, g) = normalize replaced
      let goal = fromMaybe (error "No goal") g
      -- trace (
      --   "Path: " ++ show renamed ++ "\n"
      --   ++ "WLP: " ++ show predicate ++ "\n"
      --   ++ "WLP': " ++ show replaced ++ "\n"
      --   ++ "VARS: " ++ show vars ++ "\n"
      --   ++ "ASSUMPTIONS: " ++ show assumptions ++ "\n"
      --   ++ "GOAL: " ++ show goal ++ "\n"
      --   ) $ check vars assumptions goal
      check assumptions goal
    )

fullTest1 =
  runExample 4 example @?= ["Fail", "Ignore", "Ignore", "Ignore"]

fullTest2 =
  runExample 7 minind @?= replicate 2 "Ignore"
                          ++ replicate 1 "Pass"
                          ++ replicate 2 "Ignore"
                          ++ replicate 2 "Pass"
