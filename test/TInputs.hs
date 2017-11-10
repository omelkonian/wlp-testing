module TInputs where

import Test.HUnit
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.State (evalState, forM)
import Data.SBV.Control (io)
import Data.Maybe (fromMaybe)
import Text.Parsec (parse)
import Data.SBV (runSMT)
import Prelude hiding (fail, sum)
import AST
import Parser (programP, manyProgramP)
import Paths (getAllPaths)
import Renaming (rename)
import Wlp (wlp)
import Normalizer (normalize)
import SAT
import ProgramCalls (blackBox)

inputTests =
  [ arith
  , local
  , renaming
  , assume
  , inlineAssert
  , exist
  , loop1
  , loop2
  , loopFail
  , loopInvariant
  , loopInvariant2
  , loopInvariant3
  , loopInvariant4
  , loopInvariantForall
  , loopInvariantForallFail
  , prenex
  , unfold
  , unfoldFail
  , example
  , array
  , array2
  , array3
  , array4
  , array5
  , array6
  , arrayFail
  , array2Fail
  , minind
  , minindAlt
  , maxind
  , minindFail
  , minindFail2
  , swap
  , swapFail
  , sum
  , sum2
  , sumFail
  , programs
  , programs2
  , programs3
  , programsFail
  , sort
  , sort2
  , sortFail
  ]

runInputs :: Int -> String -> [Result]
runInputs n inputFile = do
  let stmt = blackBox ps (body p)
  unsafePerformIO $ forM (take n $ getAllPaths 0 35 stmt) (\path -> do
    let renamed = evalState (rename path) 0
    let predicate = wlp renamed _T
    let (assumptions, g) = normalize predicate
    let goal = fromMaybe (error "No goal") g
    runSMT $ check 1 assumptions goal
    )
  where
    p = last ps
    ps = unsafePerformIO $ do
      program <- readFile inputFile
      case parse manyProgramP "" program of
        Left err -> error (show err)
        Right ps -> return ps

fail n input = Fail `elem` runInputs n input @?= True
pass n input = Fail `notElem` runInputs n input @?= True

arith = pass 1 "examples/arith.gcl"
local = pass 2 "examples/local.gcl"
renaming = pass 1 "examples/renaming.gcl"
assume = pass 1 "examples/assume.gcl"
inlineAssert = pass 2 "examples/assert.gcl"
exist = pass 15 "examples/exist.gcl"
loop1 = pass 5 "examples/loop1.gcl"
loop2 = pass 5 "examples/loop2.gcl"
loopFail = fail 3 "examples/loop_fail.gcl"
loopInvariant = pass 5 "examples/loop_invariant.gcl"
loopInvariant2 = pass 5 "examples/loop_invariant2.gcl"
loopInvariant3 = pass 5 "examples/loop_invariant3.gcl"
loopInvariant4 = pass 4 "examples/loop_invariant4.gcl"
loopInvariantForall = pass 4 "examples/loop_invariant_forall.gcl"
loopInvariantForallFail = fail 4 "examples/loop_invariant_forall_fail.gcl"
prenex = pass 20 "examples/prenex.gcl"
unfold = pass 8 "examples/unfold.gcl"
unfoldFail = fail 8 "examples/unfold_fail.gcl"
example = fail 5 "examples/example.gcl"
array = pass 1 "examples/array.gcl"
array2 = pass 1 "examples/array2.gcl"
array3 = pass 10 "examples/array3.gcl"
array4 = pass 1 "examples/array4.gcl"
array5 = pass 10 "examples/array5.gcl"
array6 = pass 10 "examples/array6.gcl"
arrayFail = fail 1 "examples/array_fail.gcl"
array2Fail = fail 1 "examples/array2_fail.gcl"
minind = pass 10 "examples/minind.gcl"
minindAlt = pass 10 "examples/minind_alt.gcl"
maxind = pass 10 "examples/maxind.gcl"
minindFail = fail 10 "examples/minind_fail.gcl"
minindFail2 = fail 10 "examples/minind_fail2.gcl"
swap = pass 1 "examples/swap.gcl"
swapAlt = pass 1 "examples/swapAlt.gcl"
swapFail = fail 1 "examples/swap_fail.gcl"
sum = pass 30 "examples/sum.gcl"
sum2 = pass 10 "examples/sum2.gcl"
sumFail = fail 15 "examples/sum_fail.gcl"
programs = pass 5 "examples/programs.gcl"
programs2 = pass 5 "examples/programs2.gcl"
programs3 = pass 5 "examples/programs3.gcl"
programsFail = fail 5 "examples/programs_fail.gcl"
sort = pass 5 "examples/sort.gcl"
sort2 = pass 5 "examples/sort2.gcl"
sortFail = fail 5 "examples/sort_fail.gcl"
