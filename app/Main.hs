module Main where

import System.IO
import System.Environment (getArgs)
import Control.Monad (unless)
import Data.Foldable (for_)
import Text.Parsec (parse, parseTest)
import Parser (programP)
import PrettyPrinter (pp)
import Paths (getAllPaths)

interactive = False

main :: IO ()
main = do
  inputFile <- head <$> getArgs
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
                print prog
                for_ (getAllPaths 20 prog) (\s -> do
                  putStrLn ""
                  putStrLn "=================="
                  print s)
                -- pp prog 0
