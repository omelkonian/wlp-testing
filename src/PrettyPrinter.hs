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
    put indent ["Prog: " ++ name prog ++ "("
               , commas (inputs prog) ++ "|"
               , commas (outputs prog) ++ ")"]
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
      While e s -> do
        putStrLn $ "while " ++ show e ++ " do"
        pp s (indent + 1)
      VarStmt vars s -> do
        putStrLn $ "var " ++ commas vars ++ " in"
        pp s (indent + 1)

instance PP Expr where
  pp e _ =
    case e of
      LitInt i -> putStr $ show i
      LitBool b -> putStr $ show b
      Name s -> putStr s
      Plus e1 e2 -> pp e1 0 >> putStr " + " >> pp e2 0
      Minus e1 e2 -> pp e1 0 >> putStr " - " >> pp e2 0
      And e1 e2 -> pp e1 0 >> putStr " ∩ " >> pp e2 0
      Or e1 e2 -> pp e1 0 >> putStr " ∪ " >> pp e2 0
      Imply e1 e2 -> pp e1 0 >> putStr " ~> " >> pp e2 0
      Lt e1 e2 -> pp e1 0 >> putStr " < " >> pp e2 0
      Le e1 e2 -> pp e1 0 >> putStr " <= " >> pp e2 0
      Eq e1 e2 -> pp e1 0 >> putStr " = " >> pp e2 0
      ArrayAccess s e -> putStr (s ++ "[") >> pp e 0 >> putStr "]"
      Not e -> putStr "~" >> pp e 0
      Forall (BVar s t) e -> do
        putStr ("forall " ++ s) >> pp t 0
        putStr " :: " >> pp e 0

instance PP Type where
  pp t _ = putStr $ ":" ++ show t
