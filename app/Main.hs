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
import Paths (getAllPaths)
import Renaming (rename)
import Wlp (wlp)
import Normalizer (normalize)
import SAT
import ProgramCalls (blackBox)

-- | Command-line options.
data Options = Options
  { inputFile   :: String
  , n           :: Int
  , depthStart  :: Int
  , depthEnd    :: Int
  , all         :: Bool
  }

-- | Parsing of command-line options.
parseOptions :: Parser Options
parseOptions = Options
  <$> strOption
      (  long "input"
      <> short 'i'
      <> metavar "STRING"
      <> help "GCL program file"
      )
  <*> option auto
      (  long "relevant-count"
      <> short 'n'
      <> showDefault
      <> value 10
      <> metavar "INT"
      <> help "Number of relevant test cases"
      )
  <*> option auto
      (  long "min-depth"
      <> short 's'
      <> showDefault
      <> value 1
      <> metavar "INT"
      <> help "Min depth"
      )
  <*> option auto
      (  long "max-depth"
      <> short 'e'
      <> showDefault
      <> value 100
      <> metavar "INT"
      <> help "Max depth"
      )
  <*> switch
      (  long "verify-all"
      <> short 'A'
      <> help "Whether to verify all programs"
      )

-- | Main.
main :: IO ()
main = run =<< execParser (parseOptions `withInfo` "WLP-based testing")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

-- | Run WLP testing.
run :: Options -> IO ()
run (Options inputFile n depthStart depthEnd all) = do
  hSetBuffering stdin NoBuffering
  programs <- readFile inputFile
  case parse manyProgramP "" programs of
    Left err -> print err
    Right progs ->
      for_ (if all then progs else [last progs]) (\prog -> do
        let p = prog { body = blackBox progs (body prog) }
        when all $ print p
        for_ (getAllPaths depthStart depthEnd (body p)) (\path -> do
          let renamed = evalState (rename path) 0
          let predicate = wlp renamed _T
          let (assumptions, g) = normalize predicate
          let goal = fromMaybe (error "No goal") g
          res <- runSMT $ check n assumptions goal
          let color = case res of Pass -> Green
                                  Ignore -> Yellow
                                  Fail -> Red
          setSGR [SetColor Foreground Vivid color]
          print res
          setSGR [Reset]
          )
        )
