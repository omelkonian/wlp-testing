module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Control.Monad (when, unless)
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
import Wlp (wlp, fixpointReplaceConds)
import Normalizer (normalize)
import SAT (getVars, checkAssumptions, checkGoal, checkImmediate)


data Options = Options
  { inputFile   :: String
  , depthStart  :: Int
  , depthEnd    :: Int
  , debug       :: Bool
  }

parseOptions :: Parser Options
parseOptions = Options
  <$> strOption
      (long "input" <> short 'i' <> metavar "INPUT_FILE" <> help "GCL program")
  <*> option auto
      (short 's' <> showDefault <> value 1 <> metavar "INT" <> help "Min depth")
  <*> option auto
      (short 'e' <> showDefault <> value 100 <> metavar "INT" <> help "Max depth")
  <*> switch ( long "debug" <> short 'd' <> help "Whether to print debug info" )

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

main :: IO ()
main = run =<< execParser (parseOptions `withInfo` "WLP-based testing")

run :: Options -> IO ()
run (Options inputFile depthStart depthEnd debug) = do
  program <- readFile inputFile
  case parse programP "" program of
    Left err -> print err
    Right prog -> do
      when debug $ putStrLn "------ PROGRAM -------" >> ln >> pp prog 0 >> ln
      for_ (getAllPaths depthStart depthEnd (body prog)) (\s -> do

        let renamed = evalState (rename s) 0
        when debug $ putStrLn "====== PATH =======" >> pp renamed 0 >> ln

        let predicate = wlp renamed (LitBool True)
        when debug $ putStrLn "------- WLP -------" >> pp predicate 0 >> ln
        let replaced = fixpointReplaceConds predicate
        when debug $ putStrLn "Replaced: " >> pp replaced 0 >> ln

        let (assumptions, g) = normalize replaced
        let goal = fromMaybe (error "No goal") g
        when debug (do
          putStrLn "****** GOAL *******"
          putStr "Assumptions: " >> ln
          for_ (nub assumptions) (\ass -> putStrLn $ "    " ++ show ass)
          putStr "Goal: " >> print goal)

        when debug $ putStrLn "^^^^^^ SAT ^^^^^^"
        let vars = nub $ getVars replaced ++ getVars goal
        when debug $ putStrLn $ "Vars: " ++ show vars

        runSMT $ checkImmediate vars assumptions goal

        -- solution <- runSMT $ checkAssumptions vars assumptions
        -- when debug $ putStrLn $ "Model: " ++ show solution
        -- case solution of
        --   Just result -> do
        --     res <- runSMT $ checkGoal result goal
        --     putStrLn $ if res then "Pass" else "Fail"
        --   Nothing -> putStrLn "Pass [Ignore]" -- no model for assumptions, so no relevant case
        )
