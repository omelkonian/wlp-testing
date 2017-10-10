module SAT where

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
getVars' (ArrayAccess v e) = v : getVars' e
getVars' (Forall (BVar v _) e) = v : getVars' e
getVars' (Plus e e') = getVars' e ++ getVars' e'
getVars' (Minus e e') = getVars' e ++ getVars' e'
getVars' (Imply e e') = getVars' e ++ getVars' e'
getVars' (Not e) = getVars' e
getVars' (Lt e e') = getVars' e ++ getVars' e'
getVars' (Eq e e') = getVars' e ++ getVars' e'
getVars' (Cond g et ef) = getVars' g ++ getVars' et ++ getVars' ef
getVars' (RepBy arr i e) = getVars' arr ++ getVars' i ++ getVars' e
getVars' _ = []

genSMTVars :: [String] -> Symbolic VarMap
genSMTVars vars = do smtVars <- sIntegers vars
                     return $ M.fromList $ zip vars smtVars

toSmt :: VarMap -> Expr -> SInteger
toSmt vars (LitInt i) = literal $ toInteger i
toSmt vars (Name v) = fromMaybe (error "Inconsistent VarMap") (M.lookup v vars)
toSmt vars (Plus e e') = toSmt vars e + toSmt vars e'
toSmt vars (Minus e e') = toSmt vars e - toSmt vars e'
toSmt _ _ = error "toSmt cannot handle arrays yet"

toSmtB :: VarMap -> Expr -> SBool
toSmtB vars (LitBool b) = fromBool b
toSmtB vars (Imply p q) = toSmtB vars p ==> toSmtB vars q
toSmtB vars (Not e) = bnot $ toSmtB vars e
toSmtB vars (Eq e e') = toSmt vars e .== toSmt vars e'
toSmtB vars (Lt e e') = toSmt vars e .< toSmt vars e'
toSmtB vars (Forall _ e) = toSmtB vars e
toSmtB _ _ = error "toSmtB cannot handle arrays yet"

checkAssumptions :: [String] -> [Expr] -> Symbolic (Maybe ResultMap)
checkAssumptions vars es = do
  smtVars <- genSMTVars vars
  -- Contraints
  forM_ es (constrain . toSmtB smtVars)
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



checkGoal :: ResultMap -> Expr -> Symbolic ()
checkGoal vars e = do
  vMap <- forM (M.toList vars) (\(v, xv) -> do
            x <- sInteger v
            constrain $ x .== literal xv
            return (v, x))
  query $ do
    let goal = toSmtB (M.fromList vMap) e
    io $ putStrLn ("Checking " ++ show e)
    result <- io $ putStr "Goal: " >> prove goal
    io $ print result
  return ()
