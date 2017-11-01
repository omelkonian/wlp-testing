module TInputs where

import Test.HUnit
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.State (evalState, forM)
import Data.SBV.Control (io)
import Data.Maybe (fromMaybe)
import Text.Parsec (parse)
import Data.SBV (runSMT)
import Prelude hiding (fail)

import AST
import Parser (programP)
import Paths (getAllPaths)
import Renaming (rename)
import Wlp (wlp)
import Normalizer (normalize)
import SAT (check)


inputTests =
  [ arith
  , local
  , renaming
  , inlineAssert
  , loop1
  , loop2
  , loopFail
  , loopInvariant
  , loopInvariant2
  , loopInvariant3
  , loopInvariant4
  , unfold
  , unfoldFail
  , example
  , minind
  , swap
  , swapFail
  ]

runInput :: Int -> String -> [String]
runInput n inputFile =
  unsafePerformIO $ forM allPaths (\path -> do
    let renamed = evalState (rename path) 0
    let predicate = wlp renamed _T
    let (assumptions, g) = normalize predicate
    let goal = fromMaybe (error "No goal") g
    runSMT $ check assumptions goal
    )
  where
    allPaths = take n $ getAllPaths 0 50 ex
    ex = unsafePerformIO $ do
      program <- readFile inputFile
      case parse programP "" program of
        Left err -> error (show err)
        Right prog -> return $ body prog

pass n input = "Pass" `elem` runInput n input @?= True
fail n input = "Fail" `elem` runInput n input @?= True

arith = pass 1 "examples/arith.gcl"
local = pass 2 "examples/local.gcl"
renaming = pass 1 "examples/renaming.gcl"
inlineAssert = pass 2 "examples/assert.gcl"
loop1 = pass 5 "examples/loop1.gcl"
loop2 = pass 5 "examples/loop2.gcl"
loopFail = fail 3 "examples/loop_fail.gcl"
loopInvariant = pass 5 "examples/loop_invariant.gcl"
loopInvariant2 = pass 5 "examples/loop_invariant2.gcl"
loopInvariant3 = pass 5 "examples/loop_invariant3.gcl"
loopInvariant4 = pass 4 "examples/loop_invariant4.gcl"
unfold = pass 13 "examples/unfold.gcl"
unfoldFail = fail 13 "examples/unfold_fail.gcl"
example = fail 5 "examples/example.gcl"
minind = pass 10 "examples/minind.gcl"
swap = pass 1 "examples/swap.gcl"
swapFail = fail 1 "examples/swap_fail.gcl"
