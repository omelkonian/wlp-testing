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
  pp prog indent = do
    put indent [name prog ++ ": ("
               , commas (inputs prog) ++ " | "
               , commas (outputs prog) ++ ")",
               "\n===============================\n"]
    ln
    pp (body prog) (indent + 1)

instance PP Stmt where
  pp stmt indent =
    case stmt of
      Skip -> put indent ["skip"]
      Assert e -> put indent ["assert "] >> pp e 0
      Assume e -> put indent ["assume "] >> pp e 0
      Asg asgs es -> do
        put indent [commas asgs, " := "]
        forM_ (init es) (\e -> pp e 0 >> putStr ", ")
        pp (last es) 0
      Seq s1 s2 -> do
        pp s1 indent >> putStrLn ";"
        pp s2 indent
      Ite e s1 s2 -> do
        putStrLn $ "if " ++ show e ++ " then"
        pp s1 (indent + 1) >> ln
        putStrLn "else"
        pp s2 (indent + 1)
      While _ e s -> do
        putStrLn $ "while " ++ show e ++ " do"
        pp s (indent + 1)
      VarStmt vars s -> do
        putStrLn $ "var " ++ commas vars ++ " in"
        pp s (indent + 1)

instance PP Expr where
  pp (LitInt i) _ = putStr $ show i
  pp (LitBool b) _ = putStr $ show b
  pp (Name s) _ = putStr s
  pp (Plus e1 e2) _ = pp e1 0 >> putStr " + " >> pp e2 0
  pp (Minus e1 e2) _ = pp e1 0 >> putStr " - " >> pp e2 0
  pp (Imply e1 e2) _ = pp e1 0 >> putStr " ~> " >> pp e2 0
  pp (Lt e1 e2) _ = pp e1 0 >> putStr " < " >> pp e2 0
  pp (Le e1 e2) _ = pp e1 0 >> putStr " <= " >> pp e2 0
  pp (Eq e1 e2) _ = pp e1 0 >> putStr " = " >> pp e2 0
  pp (ArrayAccess s e) _ = putStr (s ++ "[") >> pp e 0 >> putStr "]"
  pp (Not e) _ = putStr "~" >> pp e 0
  pp (Forall (BVar s t) e) _ = do
    putStr ("(forall " ++ s) >> pp t 0
    putStr " :: " >> pp e 0 >> putStr ")"
  pp (Cond g et ef) _ = do
    pp g 0
    putStr " -> " >> pp et 0
    putStr " | " >> pp ef 0
  pp (RepBy arr index rep) _ = do
    pp arr 0
    putStr "(" >> pp index 0
    putStr " repby " >> pp rep 0 >> putStr ")"

instance PP Type where
  pp t _ = putStr $ ":" ++ show t
