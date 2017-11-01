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
import System.Console.ANSI

import AST
import Parser (programP)
import PrettyPrinter (pp, ln)
import Paths (getAllPaths)
import Renaming (rename)
import Wlp (wlp)
import Normalizer (normalize)
import SAT (check)


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
    Right prog ->
      for_ (getAllPaths depthStart depthEnd (body prog)) (\path -> do
        let renamed = evalState (rename path) 0
        when debug $ putStrLn "====== PATH =======" >> pp renamed 0 >> ln
        let predicate = wlp renamed _T
        when debug $ putStrLn "------- WLP -------" >> pp predicate 0 >> ln
        let (assumptions, g) = normalize predicate
        let goal = fromMaybe (error "No goal") g
        when debug (do
          putStrLn "****** GOAL *******"
          putStr "Assumptions: " >> ln
          for_ assumptions (\ass -> putStrLn $ "    " ++ show ass)
          putStr "Goal: " >> print goal)
        res <- runSMT $ check assumptions goal
        let color = case res of "Pass" -> Green
                                "Ignore" -> Yellow
                                "Fail" -> Red
        setSGR [SetColor Foreground Vivid color]
        putStrLn res
        setSGR [Reset]
        )
