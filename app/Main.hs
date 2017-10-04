module Main where

import System.IO
import System.Environment (getArgs)
import Control.Monad (unless)
import Data.Foldable (for_)
import Text.Parsec (parse, parseTest)
import Parser (programP)
import PrettyPrinter (pp)
import Paths (getAllPaths)


main :: IO ()
main = do
  [inputFile, depth] <- getArgs
  if inputFile == "-i"
    then do hSetBuffering stdout NoBuffering
            putStr "λ> "
            program <- getLine
            unless (program == ":q") $ do
              case parse programP "" program of
                Left err -> print err
                Right prog -> pp prog 0
              main
    else do program <- readFile inputFile
            case parse programP "" program of
              Left err -> print err
              Right prog -> do
                pp prog 0
                for_ (getAllPaths (read depth) prog) (\s -> do -- (`pp` 0)
                  putStrLn ""
                  putStrLn "=================="
                  pp s 0)
