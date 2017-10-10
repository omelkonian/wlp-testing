module Main where

import System.IO
import System.Environment (getArgs)
import Control.Monad (unless)
import Control.Monad.State (evalState)
import Data.Foldable (for_)
import qualified Data.Map as M
import Text.Parsec (parse, parseTest)
import Data.SBV (runSMT, sIntegers)

import AST
import Parser (programP)
import PrettyPrinter (pp, ln)
import Paths (getAllPaths)
import Renaming (rename)
import Wlp (wlp)
import Normalizer (splitAssumptions)
import SAT (getVars, checkAssumptions, checkGoal)


main :: IO ()
main = do
  [inputFile, depth] <- getArgs
  program <- readFile inputFile
  case parse programP "" program of
    Left err -> print err
    Right prog -> do
      pp prog 0 >> ln
      for_ (getAllPaths (read depth) prog) (\s -> do

        putStrLn "====== PATH ======="
        let renamed = evalState (rename s) 0
        pp renamed 0 >> ln
        putStrLn "==================" >> ln

        putStrLn "------- WLP -------"
        let wlp_ = wlp renamed (LitBool True)
        pp wlp_ 0 >> ln
        putStrLn "-------------------" >> ln

        putStrLn "****** GOAL *******"
        let (assumptions, goal) = splitAssumptions wlp_
        print assumptions
        print goal
        putStrLn "*******************" >> ln

        putStrLn "^^^^^^ SAT ^^^^^^"
        let vars = getVars goal
        solution <- runSMT $ checkAssumptions vars assumptions
        putStrLn $ "Model: " ++ show solution
        case solution of
          Just result -> runSMT $ checkGoal result goal
          Nothing -> putStrLn "Pass" -- no model for assumptions, so no relevant case
        putStrLn "^^^^^^^^^^^^^^^^^^^" >> ln
        )
