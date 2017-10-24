module SAT where

import Control.Monad
import Data.SBV
import Data.SBV.Control hiding (Name)
import Data.List hiding (lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import AST

type SArr = SFunArray Integer Integer
type VarMap = M.Map String SInteger
type ArrayMap = M.Map String SArr
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

getArrays :: Expr -> [String]
getArrays e = nub $ getArrays' e

getArrays' :: Expr -> [String]
getArrays' (ArrayAccess v _) = [v]
getArrays' (Forall _ e) = getArrays' e
getArrays' (Plus e e') = getArrays' e ++ getArrays' e'
getArrays' (Minus e e') = getArrays' e ++ getArrays' e'
getArrays' (Imply e e') = getArrays' e ++ getArrays' e'
getArrays' (Not e) = getArrays' e
getArrays' (Lt e e') = getArrays' e ++ getArrays' e'
getArrays' (Eq e e') = getArrays' e ++ getArrays' e'
getArrays' (Cond g et ef) = getArrays' g ++ getArrays' et ++ getArrays' ef
getArrays' _ = []

genSMTArrays :: [String] -> Symbolic ArrayMap
genSMTArrays vars = do smtVars <- mapM newArray vars
                       return $ M.fromList $ zip vars smtVars

toSmt :: VarMap -> ArrayMap -> Expr -> SInteger
toSmt vs as (LitInt i) = literal $ toInteger i
toSmt vs as (Name v) =
  fromMaybe (error "Inconsistent VarMap") (M.lookup v vs)
toSmt vs as (Plus e e') = toSmt vs as e + toSmt vs as e'
toSmt vs as (Minus e e') = toSmt vs as e - toSmt vs as e'
toSmt vs as (ArrayAccess a e) =
  readArray array index
  where array = fromMaybe (error "Inconsistent ArrayMap") (M.lookup a as)
        index = toSmt vs as e
toSmt _ _ _ = error "toSmt cannot handle logical expressions"

toSmtB :: VarMap -> ArrayMap -> Expr -> SBool
toSmtB vs as (LitBool b) = fromBool b
toSmtB vs as (Imply p q) = toSmtB vs as p ==> toSmtB vs as q
toSmtB vs as (Not e) = bnot $ toSmtB vs as e
toSmtB vs as (Eq e e') = toSmt vs as e .== toSmt vs as e'
toSmtB vs as (Lt e e') = toSmt vs as e .< toSmt vs as e'
toSmtB vs as (Forall _ e) = toSmtB vs as e
toSmtB _ _ _ = error "toSmtB cannot handle arithmetic expressions"

checkAssumptions :: [String] -> [String] -> [Expr] -> Symbolic (Maybe ResultMap)
checkAssumptions vars arrays assumptions = do
  smtVars <- genSMTVars vars
  smtArrays <- genSMTArrays arrays
  -- Contraints
  forM_ assumptions (constrain . toSmtB smtVars smtArrays)
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



-- checkGoal :: ResultMap -> Expr -> Symbolic Bool
-- checkGoal vars e = do
--   vMap <- forM (M.toList vars) (\(v, xv) -> do
--             x <- sInteger v
--             constrain $ x .== literal xv
--             return (v, x))
--   query $ do
--     constrain $ toSmtB (M.fromList vMap) e
--     cs <- checkSat
--     case cs of
--       Unk   -> error "Unknown"
--       Unsat -> return False
--       Sat   -> return True
