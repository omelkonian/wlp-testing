module SAT where

import System.Console.ANSI
import Control.Monad
import Data.SBV
import Data.SBV.Control hiding (Name)
import Data.List hiding (lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import AST


type VarMap = M.Map String SInteger
type ResultMap = M.Map String Integer

getVars :: Expr -> [String]
getVars e = nub $ getVars' e

getVars' :: Expr -> [String]
getVars' (Name v) = [v]
getVars' (ArrayAccess v e) = getVars' e
getVars' (Forall vs e) = vs ++ getVars' e
getVars' (Plus e e') = getVars' e ++ getVars' e'
getVars' (Minus e e') = getVars' e ++ getVars' e'
getVars' (Imply e e') = getVars' e ++ getVars' e'
getVars' (Not e) = getVars' e
getVars' (Lt e e') = getVars' e ++ getVars' e'
getVars' (Eq e e') = getVars' e ++ getVars' e'
getVars' (Cond g et ef) = getVars' g ++ getVars' et ++ getVars' ef
getVars' _ = []

genSMTVars :: [String] -> Symbolic VarMap
genSMTVars vars = do smtVars <- sIntegers vars
                     return $ M.fromList $ zip vars smtVars

toSmt :: VarMap -> Expr -> SInteger
toSmt vs (LitInt i) = literal $ toInteger i
toSmt vs (Name v) =
  fromMaybe (error "Inconsistent VarMap") (M.lookup v vs)
toSmt vs (Plus e e') = toSmt vs e + toSmt vs e'
toSmt vs (Minus e e') = toSmt vs e - toSmt vs e'
toSmt vs (ArrayAccess a e) = uninterpret a (toSmt vs e)
toSmt _ _ = error "toSmt cannot handle logical expressions"

toSmtB :: VarMap -> Expr -> SBool
toSmtB vs (LitBool b) = fromBool b
toSmtB vs (Imply p q) = toSmtB vs p ==> toSmtB vs q
toSmtB vs (Not e) = bnot $ toSmtB vs e
toSmtB vs (Eq e e') = toSmt vs e .== toSmt vs e'
toSmtB vs (Lt e e') = toSmt vs e .< toSmt vs e'
toSmtB vs (Forall _ e) = toSmtB vs e
toSmtB _ _ = error "toSmtB cannot handle arithmetic expressions"

checkAssumptions :: [String] -> [Expr] -> Symbolic (Maybe ResultMap)
checkAssumptions vars assumptions = do
  smtVars <- genSMTVars vars
  -- Contraints
  forM_ assumptions (constrain . toSmtB smtVars)
  -- Query
  query $ do
    cs <- checkSat
    case cs of
      Unk   -> error "Solver said unknown!"
      Unsat -> return Nothing -- no solution!
      Sat   -> do res <- forM (M.toList smtVars) (\(v, x) -> do
                    xv <- getValue x
                    return (v, xv))
                  return $ Just $ M.fromList res

checkGoal :: ResultMap -> Expr -> Symbolic Bool
checkGoal vars e = do
  vMap <- forM (M.toList vars) (\(v, xv) -> do
            x <- sInteger v
            constrain $ x .== literal xv
            return (v, x))
  constrain $ toSmtB (M.fromList vMap) e
  query $ do
    cs <- checkSat
    case cs of
      Unk   -> error "Unknown"
      Unsat -> return False
      Sat   -> return True

checkImmediate :: [String] -> [Expr] -> Expr -> Symbolic ()
checkImmediate vars assumptions goal = do
  smtVars <- genSMTVars vars
  forM_ assumptions (constrain . toSmtB smtVars)
  -- let ass = foldl1 (&&&) (map (toSmtB smtVars) assumptions)
  -- constrain ass
  let g = toSmtB smtVars goal
  constrain g
  -- Query
  query $ do
    cs <- checkSat
    case cs of
      Unk   -> error "Unknown!"
      Unsat -> io $ runSMT $ query $ do
        res <- io $ prove $ foldl1 (&&&) (map (toSmtB smtVars) assumptions)
        case show res of
          "Falsifiable" -> io $ do
            setSGR [SetColor Foreground Vivid Yellow]
            putStrLn "Ignore"
            setSGR [Reset]
          _ -> io $ do
            setSGR [SetColor Foreground Vivid Red]
            putStrLn "Fail"
            setSGR [Reset]
        -- forM_ assumptions (constrain . toSmtB smtVars)
        -- cs' <- checkSat
        -- case cs' of
        --   Unk   -> error "Unknown"
        --   Unsat -> return "Ignore"
        --   Sat   -> return "Fail"
      Sat   -> io $ do
        setSGR [SetColor Foreground Vivid Green]
        putStrLn "Pass"
        setSGR [Reset]
