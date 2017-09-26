module Main where

import System.IO
import System.Environment (getArgs)
import Control.Monad (unless)
import Text.Parsec (parseTest)
import Parser (programP)

interactive = False

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  if inputFile == "-i"
    then do hSetBuffering stdout NoBuffering
            putStr "λ> "
            program <- getLine
            unless (program == ":q") $ do
              parseTest programP program
              main
    else do program <- readFile inputFile
            parseTest programP program
