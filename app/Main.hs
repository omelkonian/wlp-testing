module Main where

import System.IO
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
import Parser (programP, manyProgramP)
import PrettyPrinter (pp, ln)
import Paths (getAllPaths)
import Renaming (rename)
import Wlp (wlp)
import Normalizer (normalize)
import SAT (check)
import ProgramCalls (blackBox)

-- | Command-line options.
data Options = Options
  { inputFile   :: String
  , depthStart  :: Int
  , depthEnd    :: Int
  , debug       :: Bool
  , all         :: Bool
  }

-- | Parsing of command-line options.
parseOptions :: Parser Options
parseOptions = Options
  <$> strOption
      (long "input" <> short 'i' <> metavar "INPUT_FILE" <> help "GCL program")
  <*> option auto
      (short 's' <> showDefault <> value 1 <> metavar "INT" <> help "Min depth")
  <*> option auto
      (short 'e' <> showDefault <> value 100 <> metavar "INT" <> help "Max depth")
  <*> switch (long "debug" <> short 'd' <> help "Whether to print debug info")
  <*> switch (long "all" <> short 'A' <> help "Whether to verify all programs")

-- | Main.
main :: IO ()
main = run =<< execParser (parseOptions `withInfo` "WLP-based testing")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

-- | Run WLP testing.
run :: Options -> IO ()
run (Options inputFile depthStart depthEnd debug all) = do
  hSetBuffering stdin NoBuffering
  programs <- readFile inputFile
  case parse manyProgramP "" programs of
    Left err -> print err
    Right progs ->
      for_ (if all then progs else [last progs]) (\prog -> do
        let p = prog { body = blackBox progs (body prog) }
        when all $ pp p 0 >> pp (body p) 0 >> ln
        for_ (getAllPaths depthStart depthEnd (body p)) (\path -> do
          let renamed = evalState (rename path) 0
          when debug $ putStrLn "\n------- PATH -------" >> pp renamed 0 >> ln
          let predicate = wlp renamed _T
          when debug $ putStrLn "\n------- WLP -------" >> pp predicate 0 >> ln
          let (assumptions, g) = normalize predicate
          let goal = fromMaybe (error "No goal") g
          when debug (do
            putStrLn "\n------- GOAL -------"
            putStr "Assumptions: " >> ln
            for_ assumptions (\ass -> putStrLn $ "\t" ++ show ass)
            putStr "Goal: " >> ln >> putStr "\t" >> print goal)
          res <- runSMT $ check debug assumptions goal
          let color = case res of "Pass" -> Green
                                  "Ignore" -> Yellow
                                  "Fail" -> Red
          setSGR [SetColor Foreground Vivid color]
          putStrLn res
          setSGR [Reset]
          )
        )
