{-# LANGUAGE FlexibleContexts #-}
module SAT where

import System.Console.ANSI
import System.IO.Unsafe
import Control.Monad
import Control.Arrow (second)
import Data.SBV hiding (or)
import Data.SBV.Control hiding (Name)
import Data.SBV.Internals (CW, modelAssocs)
import Data.List hiding (lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import AST hiding ((==>), (.<), (.>), name)
import Wlp (subst)
import Normalizer (normalize')

import Debug.Trace


type VarMap = M.Map String SInteger
type UVarMap = M.Map String (SInteger -> SInteger)
type ResultMap = M.Map String Integer

getManyVars :: [Expr] -> [String]
getManyVars e = nub $ concatMap getVars e
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

getManyUVars :: [Expr] -> [String]
getManyUVars e = nub $ concatMap getUVars e
getUVars :: Expr -> [String]
getUVars e = nub $ getUVars' e
getUVars' :: Expr -> [String]
getUVars' (BinOp _ e e') = getUVars' e ++ getUVars' e'
getUVars' (Not e) = getUVars' e
getUVars' (Cond g et ef) = getUVars' g ++ getUVars' et ++ getUVars' ef
getUVars' (Forall _ e) = getUVars' e
getUVars' (ArrayAccess v e) = v : getUVars' e
getUVars' _ = []

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

genVars :: [String] -> Symbolic VarMap
genVars vars = do
  smtVars <- sIntegers vars
  return $ M.fromList $ zip vars smtVars

genUVars :: [String] -> Symbolic UVarMap
genUVars vars = do
  let f = map uninterpret vars
  return $ M.fromList $ zip vars f

toSmt :: (VarMap, UVarMap) -> Expr -> SInteger
toSmt vs (LitInt i) = literal $ toInteger i
toSmt (vs, _) (Name v) =
  fromMaybe (
    error $ "Inconsistent VarMap: " ++ show v ++ " does not appear in " ++ show vs
    ) (M.lookup v vs)
toSmt vs (BinOp Plus e e') = toSmt vs e + toSmt vs e'
toSmt vs (BinOp Minus e e') = toSmt vs e - toSmt vs e'
toSmt v@(_, uvs) (ArrayAccess a e) = u (toSmt v e)
  where
    u = fromMaybe (error $ "Inconsistent UVarMap: " ++ show a) (M.lookup a uvs)
toSmt _ _ = error "toSmt cannot handle logical expressions"

toSmtB :: (VarMap, UVarMap) -> Expr -> SBool
toSmtB vs (LitBool b) = fromBool b
toSmtB vs (BinOp Eq e e') = toSmt vs e .== toSmt vs e'
toSmtB vs (BinOp Lt e e') = toSmt vs e .< toSmt vs e'
toSmtB vs (Not e) = bnot $ toSmtB vs e
toSmtB vs (BinOp Imply p q) = toSmtB vs p ==> toSmtB vs q
toSmtB vs (Forall _ e) = toSmtB vs e
toSmtB _ _ = error "toSmtB cannot handle arithmetic expressions"

toGSmt :: (ResultMap, VarMap, UVarMap) -> Expr -> SInteger
toGSmt vs (LitInt i) = literal $ toInteger i
toGSmt (model, vs, _) (Name v) =
  case M.lookup v model of
    Nothing -> fromMaybe (error "Inconsistent") (M.lookup v vs)
    Just i -> literal $ toInteger i
toGSmt vs (BinOp Plus e e') = toGSmt vs e + toGSmt vs e'
toGSmt vs (BinOp Minus e e') = toGSmt vs e - toGSmt vs e'
toGSmt v@(_, _, uvs) (ArrayAccess a e) = u $ toGSmt v e
  where
    u = fromMaybe (error $ "Inconsistent UVarMap: " ++ show a) (M.lookup a uvs)
toGSmt _ _ = error "toSmt cannot handle logical expressions"

toGSmtB :: (ResultMap, VarMap, UVarMap) -> Expr -> SBool
toGSmtB vs (LitBool b) = fromBool b
toGSmtB vs (BinOp Eq e e') = toGSmt vs e .== toGSmt vs e'
toGSmtB vs (BinOp Lt e e') = toGSmt vs e .< toGSmt vs e'
toGSmtB vs (Not e) = bnot $ toGSmtB vs e
toGSmtB vs (BinOp Imply p q) = toGSmtB vs p ==> toGSmtB vs q
toGSmtB vs (Forall _ e) = toGSmtB vs e
toGSmtB _ _ = error "toSmtB cannot handle arithmetic expressions"

containsArray :: Expr -> Bool
containsArray ArrayAccess {} = True
containsArray (BinOp _ e e') = containsArray e || containsArray e'
containsArray (Not e) = containsArray e
containsArray (Forall _ e) = containsArray e
containsArray _ = False

checkAssumptions :: (VarMap, UVarMap) -> [Expr] -> Symbolic (Maybe ResultMap)
checkAssumptions vs@(vars, _) assumptions = do
  -- Contraints
  forM_ assumptions (constrain . toSmtB vs)
  -- Query
  query $ do
    cs <- checkSat
    case cs of
      Unk   -> error "Undecidable!"
      Unsat -> return Nothing
      Sat   -> do res <- forM (M.toList vars) (\(v, x) -> do
                    xv <- getValue x
                    return (v, xv))
                  return $ Just $ M.fromList res

assign :: ResultMap -> Expr -> Expr
assign model = subst vs es
  where (vs, es) = unzip $ map (second $ LitInt . fromInteger) $ M.toList model

checkGoal :: (ResultMap, VarMap, UVarMap) -> Expr -> Symbolic Bool
checkGoal vs@(model, _, _) g = do
  -- Constraint
  let g' = assign model g
  let b = toGSmtB vs g'
  case show b of
    "True" -> return True
    "False" -> return False
    _ -> case show b of
           "True" -> return True
           "False" -> return False
           _ -> query $ do
                res <- io $ prove b
                io $ print res
                case show res of
                  "Falsifiable" -> return False
                  _ -> return True

check :: [Expr] -> Expr -> Symbolic String
check assumptions goal = do
  -- Set logic
  -- setLogic UFNIA
  -- Generate vars
  let vars = getManyVars assumptions
  let uVars = getManyUVars assumptions
  let res =
        unsafePerformIO $ runSMT $ do
          smtVars <- genVars vars
          smtUVars <- genUVars uVars
          checkAssumptions (smtVars, smtUVars) assumptions
  case res of
    Nothing -> return "Ignore"
    Just model -> do
      -- Model assignment
      let goal' = assign model goal
      let ass' = map (assign model) $ filter containsArray assumptions
      -- Re-normalize
      let (newAssumptions, newGoal) = normalize' goal'
      -- Query
      query $ do
        -- io $ do
        --   putStrLn $ "Assumptions: " ++ show assumptions
        --   putStrLn $ "Goal: " ++ show goal
        --   putStrLn $ "Model: " ++ show model
        --   putStrLn $ "Assumptions2: " ++ show ass'
        --   putStrLn $ "Goal2: " ++ show goal'
        --   putStrLn $ "NewAssumptions: " ++ show newAssumptions
        --   putStrLn $ "NewGoal: " ++ show newGoal

        res <- io $ proveZ $ do
          -- Generation
          let gVars = getVars newGoal
          let gUVars = getUVars newGoal
          gSmtVars <- genVars gVars
          gSmtUVars <- genUVars gUVars
          -- Assumptions
          let args = (model, gSmtVars, gSmtUVars)
          let smtAss = foldl (&&&) true $ map (toGSmtB args) (ass' ++ newAssumptions)
          constrain smtAss
          -- Goal
          return $ toGSmtB args newGoal
          
        case show res of
          "Q.E.D." -> return "Pass"
          _ -> return "Fail"

proveZ :: Provable a => a -> IO ThmResult
proveZ = proveWith z3{verbose=False}
