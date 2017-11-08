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
import Normalizer (toPrenexFormFixpoint)

-- | Type aliases.
type Vars = ( [String] -- free variables
            , [String] -- universally-quantified variables
            , [String] -- existentially-quantified variables
            , [String] -- uninterpreted variables
            )
type VarMap = M.Map String SInteger
type UVarMap = M.Map String (SInteger -> SInteger)
type ResultMap = M.Map String Integer

-- | Extract all types of variables from an expression.
getManyVars :: [Expr] -> Vars
getManyVars es =
  let (vs, fvs, evs, uvs) = unzip4 $ map getVars es
  in (nub $ concat vs, nub $ concat fvs, nub $ concat evs, nub $ concat uvs)
getVars :: Expr -> Vars
getVars e =
  let (vs, fvs, evs, uvs) = getVars' e
      freeVars = nub vs \\ (fvs ++ evs)
  in (freeVars, nub fvs, nub evs, nub uvs)
getVars' :: Expr -> Vars
getVars' (Name v) = ([v], [], [], [])
getVars' (BinOp _ e e') =
  let (vs, fvs, evs, uvs) = getVars' e
      (vs', fvs', evs', uvs') = getVars' e'
  in (vs ++ vs', fvs ++ fvs', evs ++ evs', uvs ++ uvs')
getVars' (Not e) = getVars' e
getVars' (Cond g et ef) =
  let (vs, fvs, evs, uvs) = getVars' g
      (vs', fvs', evs', uvs') = getVars' et
      (vs'', fvs'', evs'', uvs'') = getVars' ef
  in (vs ++ vs' ++ vs'', fvs ++ fvs' ++ fvs'', evs ++ evs' ++ evs'', uvs ++ uvs' ++ uvs'')
getVars' (Forall v e) =
  let (vs, fvs, evs, uvs) = getVars' e
  in (vs, v ++ fvs, evs, uvs)
getVars' (Exist v e) =
  let (vs, fvs, evs, uvs) = getVars' e
  in (vs, fvs, v ++ evs, uvs)
getVars' (ArrayAccess v e) =
  let (vs, fvs, evs, uvs) = getVars' e
  in (vs, fvs, evs, v : uvs)
getVars' _ = ([], [], [], [])

-- | Generate SBV variables.
genVars :: Vars -> Symbolic (VarMap, UVarMap)
genVars (vs, fvs, evs, uvs) = do
  vs' <- sIntegers $ vs ++ fvs
  evs' <- mapM exists evs
  let uvs' = map uninterpret uvs
  return (M.fromList $ zip (vs ++ fvs ++ evs) (vs' ++ evs'),
          M.fromList $ zip uvs uvs')

-- | Convert a GCL arithmetic expression to the corresponding SBV expression.
toSmt :: (VarMap, UVarMap) -> Expr -> Symbolic SInteger
toSmt vs (LitInt i) = return $ literal (toInteger i)
toSmt (vs, _) (Name v) = return $
  fromMaybe (error $ "Inconsistent VarMap: " ++ show v ++ " not in " ++ show vs) (M.lookup v vs)
toSmt vs (BinOp op e e') = do
  ve <- toSmt vs e
  ve' <- toSmt vs e'
  return $ smtOp op ve ve'
toSmt v@(_, uvs) (ArrayAccess a e) = do
  v <- toSmt v e
  return $ u v
  where
    u = fromMaybe (error $ "Inconsistent UVarMap: " ++ show a) (M.lookup a uvs)
toSmt vs (Cond g et ef) = do
  x <- free_
  vg <- toSmtB vs g
  vt <- toSmt vs et
  vf <- toSmt vs ef
  constrain $ vg ||| bnot vg
  constrain $ vg ==> x .== vt
  constrain $ bnot vg ==> x .== vf
  return x
toSmt _ _ = error "toSmt cannot handle logical expressions"

smtOp Plus = (+)
smtOp Minus = (-)

-- | Convert a GCL logical expression to the corresponding SBV expression.
toSmtB :: (VarMap, UVarMap) -> Expr -> Symbolic SBool
toSmtB vs (LitBool b) = return $ fromBool b
toSmtB vs (BinOp op e e') =
  case op of
    Imply -> do
      ve <- toSmtB vs e
      ve' <- toSmtB vs e'
      return $ ve ==> ve'
    _ -> do
      ve <- toSmt vs e
      ve' <- toSmt vs e'
      return $ smtOpB op ve ve'
toSmtB vs (Not e) = do
  v <- toSmtB vs e
  return $ bnot v
toSmtB vs (Forall v e) = toSmtB vs e
toSmtB vs (Exist v e) = toSmtB vs e
toSmtB _ e = error $ "toSmtB cannot handle arithmetic expressions: " ++ show e

smtOpB Eq = (.==)
smtOpB Lt = (.<)

-- | Check whether an assumption reasons about arrays.
containsArray :: Expr -> Bool
containsArray ArrayAccess {} = True
containsArray (BinOp _ e e') = containsArray e || containsArray e'
containsArray (Not e) = containsArray e
containsArray (Forall _ e) = containsArray e
containsArray (Exist _ e) = containsArray e
containsArray _ = False

-- | Assign a model (i.e. a result from the SAT solver) to a logical expression.
assign :: ResultMap -> Expr -> Expr
assign model = subst vs es
  where (vs, es) = unzip $ map (second $ LitInt . fromInteger) $ M.toList model

-- | Check whether the program's annotated specification is valid.
check :: [Expr] -> Expr -> Symbolic String
check assumptions goal = do
  -- Set logic
  setLogic UFNIA
  -- Generate vars
  let vars = getManyVars assumptions
  let res =
        unsafePerformIO $ runSMT $ do
          (smtVars, smtUVars) <- genVars vars
          -- Contraints
          assumptions' <- mapM (toSmtB (smtVars, smtUVars)) assumptions
          forM_ assumptions' constrain
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
  case res of
    Nothing -> return "Ignore"
    Just model -> do
      -- Model assignment + Prenex conversion
      let goal' = toPrenexFormFixpoint $ assign model goal
      let assumptions' = map (assign model) $ filter containsArray assumptions
      let vars = getManyVars (goal' : assumptions')
      query $ do
        -- io $ do
        --  putStrLn $ "Model: " ++ show model
        --  putStrLn $ "Assumptions: " ++ show assumptions'
        --  putStrLn $ "Goal: " ++ show goal'
        res <- io $ proveWith z3{verbose=False} $ do
          -- Generation
          vars <- genVars $ getManyVars (goal' : assumptions')
          -- Assumptions (needed for uninterpreted vars i.e. involving arrays)
          ass' <- mapM (toSmtB vars) assumptions'
          forM_ ass' constrain
          -- Goal
          toSmtB vars goal'
        -- io $ print res
        case show res of
          "Q.E.D." -> return "Pass"
          _ -> return "Fail"
