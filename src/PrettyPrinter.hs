module PrettyPrinter where

import Control.Monad
import Data.List
import AST

ln = putStrLn ""
commas = intercalate ", "
put indent l = putStr $ concat $ replicate indent "    " ++ l

class PP a where
  pp :: a -> Int -> IO ()

instance PP Program where
  pp prog _ = do
    ln >> putStrLn "==============================="
    put 0 ["\t", name prog, ": (",
           commas (inputs prog), " | ", commas (outputs prog), ")"] >> ln
    putStrLn "==============================="
    pp (body prog) 1 >> ln >> ln

instance PP Stmt where
  pp stmt indent =
    case stmt of
      Skip -> put indent ["skip"]
      Assert e -> put indent ["assert "] >> putStr (show e)
      Assume e -> put indent ["assume "] >> putStr (show e)
      Asg asgs es -> do
        put indent [commas asgs, " := "]
        forM_ (init es) (\e -> putStr $ show e ++ ", ")
        putStr $ show (last es)
      Seq s1 s2 ->
        pp s1 indent >> putStrLn ";" >> pp s2 indent
      Ite e s1 s2 -> do
        put indent ["if (", show e, ") then"] >> ln
        pp s1 (indent + 1) >> ln
        put indent ["else"] >> ln
        pp s2 (indent + 1)
      While _ e s -> do
        put indent ["while (", show e, ") do"] >> ln
        pp s (indent + 1) >> ln
        put indent ["end"]
      VarStmt vars s -> do
        put indent ["var (", commas vars, ") in"] >> ln
        pp s (indent + 1)

instance PP Expr where
  pp (BinOp Imply e1 e2) i = do
    pp e1 i >> putStr " => (" >> ln
    pp e2 (i + 1) >> ln
    put i [")"]
  pp (Forall vs e) _ = do
    putStr $ "(forall " ++ show vs
    putStr " :: " >> pp e 0 >> putStr ")"
  pp (Cond g et ef) _ = do
    putStr "(" >> pp g 0
    putStr " -> " >> pp et 0
    putStr " | " >> pp ef 0 >> putStr ")"
  pp e i = put i [show e]
