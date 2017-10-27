module SAT where

import System.Console.ANSI
import System.IO.Unsafe
import Control.Monad
import Data.SBV
import Data.SBV.Control hiding (Name)
import Data.List hiding (lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import AST hiding ((==>), (.<))

import Debug.Trace


type VarMap = M.Map String SInteger
type ResultMap = M.Map String Integer

getVars :: Expr -> [String]
getVars e = nub $ getVars' e

getVars' :: Expr -> [String]
getVars' (Name v) = [v]
getVars' (BinOp _ e e') = getVars' e ++ getVars' e'
getVars' (Not e) = getVars' e
getVars' (Cond g et ef) = getVars' g ++ getVars' et ++ getVars' ef
getVars' (Forall vs e) = vs ++ getVars' e
getVars' (ArrayAccess v e) = getVars' e
getVars' _ = []

getBoundVars :: Expr -> [String]
getBoundVars e = nub $ getBoundVars' e

getBoundVars' :: Expr -> [String]
getBoundVars' (BinOp _ e e') = getBoundVars' e ++ getBoundVars' e'
getBoundVars' (Not e) = getBoundVars' e
getBoundVars' (Cond g et ef) = getBoundVars' g ++ getBoundVars' et ++ getBoundVars' ef
getBoundVars' (Forall vs e) = vs ++ getBoundVars' e
getBoundVars' (ArrayAccess v e) = getBoundVars' e
getBoundVars' _ = []

getFreeVars :: Expr -> [String]
getFreeVars e = getVars e \\ getBoundVars e

genSMTVars :: [String] -> Symbolic VarMap
genSMTVars vars = do
  smtVars <- sIntegers vars
  return $ M.fromList $ zip vars smtVars

toSmt :: VarMap -> Expr -> SInteger
toSmt vs (LitInt i) = literal $ toInteger i
toSmt vs (Name v) =
  fromMaybe (
    error $ "Inconsistent VarMap: " ++ show v ++ " does not appear in " ++ show vs
    ) (M.lookup v vs)
toSmt vs (BinOp Plus e e') = toSmt vs e + toSmt vs e'
toSmt vs (BinOp Minus e e') = toSmt vs e - toSmt vs e'
toSmt vs (ArrayAccess a e) = uninterpret a (toSmt vs e)
toSmt _ _ = error "toSmt cannot handle logical expressions"

toSmtB :: VarMap -> Expr -> SBool
toSmtB vs (LitBool b) = fromBool b
toSmtB vs (BinOp Eq e e') = toSmt vs e .== toSmt vs e'
toSmtB vs (BinOp Lt e e') = toSmt vs e .< toSmt vs e'
toSmtB vs (Not e) = bnot $ toSmtB vs e
toSmtB vs (BinOp Imply p q) = toSmtB vs p ==> toSmtB vs q
toSmtB vs (Forall _ e) = toSmtB vs e
toSmtB _ _ = error "toSmtB cannot handle arithmetic expressions"

checkAssumptions :: [String] -> [Expr] -> Symbolic (Maybe ResultMap)
checkAssumptions vars assumptions = do
  -- Generate vars
  smtVars <- genSMTVars vars
  -- Contraints
  forM_ assumptions (constrain . toSmtB smtVars)
  -- Query
  query $ do
    cs <- checkSat
    case cs of
      Unk   -> error "Undecidable!"
      Unsat -> return Nothing
      Sat   -> do res <- forM (M.toList smtVars) (\(v, x) -> do
                    xv <- getValue x
                    return (v, xv))
                  return $ Just $ M.fromList res

checkGoal :: ResultMap -> Expr -> Symbolic Bool
checkGoal vars g = do
  -- Model assignment
  vMap <- forM (M.toList vars) (\(v, xv) -> do
            x <- sInteger v
            constrain $ x .== literal xv
            return (v, x))
  -- Generate vars
  let genVars = getVars g \\ map fst (M.toList vars)
  smtVars <- genSMTVars genVars
  let allVars = M.union (M.fromList vMap) smtVars

  -- Constraint
  constrain $ toSmtB allVars g
  -- Query
  query $ do
    cs <- checkSat
    case cs of
      Unk   -> error "Undecidable!"
      Unsat -> return False
      Sat   -> return True

check :: [String] -> [Expr] -> Expr -> Symbolic String
check vars assumptions goal = do
  let res = unsafePerformIO $ runSMT $ checkAssumptions vars assumptions
  case res of
    Nothing -> return "Ignore"
    Just model ->
      if not (all (`elem` vars) (getFreeVars goal)) then
        return "Fail"
      else do
        let ans = unsafePerformIO $ runSMT $ checkGoal model goal
        if ans then return "Pass" else return "Fail"

checkImmediate :: [String] -> [Expr] -> Expr -> Symbolic ()
checkImmediate vars assumptions goal = do
  smtVars <- genSMTVars vars
  forM_ assumptions (constrain . toSmtB smtVars)
  let g = toSmtB smtVars goal
  constrain g
  -- Query
  query $ do
    cs <- checkSat
    case cs of
      Unk   -> error "Undecidable!"
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
      Sat   -> io $ do
        setSGR [SetColor Foreground Vivid Green]
        putStrLn "Pass"
        setSGR [Reset]
