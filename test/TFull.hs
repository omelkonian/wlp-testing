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

fullTests =
  [ fullTest1
  , fullTest2
  , fullTest3
  , fullTest4
  , fullTest5
  ]

runExample :: Int -> Stmt -> [String]
runExample n ex =
  unsafePerformIO $ forM allPaths (\path -> do
    let renamed = evalState (rename path) 0
    let predicate = wlp renamed _T
    let (assumptions, g) = normalize predicate
    let goal = fromMaybe (error "No goal") g
    runSMT $ check assumptions goal
    )
  where
    allPaths = take n $ getAllPaths 0 50 ex

fullTest1 =
  runExample 4 example @?= ["Fail", "Ignore", "Ignore", "Ignore"]

fullTest2 =
  runExample 4 example2 @?= replicate 4 "Pass"

fullTest3 =
  runExample 15 minind @?= replicate 2 "Ignore"
                          ++ replicate 1 "Pass"
                          ++ replicate 2 "Ignore"
                          ++ replicate 2 "Pass"
                          ++ replicate 4 "Ignore"
                          ++ replicate 4 "Pass"

fullTest4 =
  runExample 15 loopInvariant @?= replicate 15 "Pass"

fullTest5 =
  runExample 1 swap @?= ["Pass"]
