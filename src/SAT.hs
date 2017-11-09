module SAT (check) where

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
import Normalizer (prenexFixpoint)

import Debug.Trace

-- | Type aliases.
type Vars = ( [String] -- free variables
            , [String] -- universally-quantified variables
            , [String] -- existentially-quantified variables
            , [String] -- uninterpreted variables
            )
type SBVars = ( M.Map String SInteger -- free variables
              , M.Map String SInteger -- universally-quantified variables
              , M.Map String SInteger -- existentially-quantified variables
              , M.Map String (SInteger -> SInteger) -- uninterpreted variables
              )
type ResultMap = M.Map String Integer

-- | Check whether the program's annotated specification is valid.
check :: [Expr] -> Expr -> Symbolic String
check a g = do
  let (assumptions, goal) = (map sanitizeE a, sanitizeE g)
  -- Set logic
  setLogic UFNIA

  -- Generate vars
  let ass = map prenexFixpoint assumptions
  let assVars = getManyVars ass
  assVars'@(assVs, _, _, _) <- genVars assVars
  -- Contraints
  assumptions' <- mapM (toSmtB assVars') ass
  forM_ assumptions' constrain
  -- Query
  query $ do
    cs <- checkSat
    case cs of
      Unk   -> error "Undecidable!"
      Unsat -> return "Ignore"
      Sat   -> do
        -- Check if input assumptions are satisfiable (i.e. path is feasible)
        let resultVars = filter (\(v, _) -> head v /= '$') $ M.toList assVs
        res <- forM resultVars (\(v, x) -> do
          xv <- getValue x
          return (v, xv))
        let model = M.fromList res
        -- Model assignment (i.e. relevant test case)
        let goal' = assign model goal
        let forallAss = map (assign model) $ filter (containsUVar $ map fst res) ass
        let arrayAss = map (assign model) $ filter containsArray ass
        let ass' = nub $ forallAss ++ arrayAss
        let newGoal =
              prenexFixpoint $ BinOp Imply (foldr (/\) _T ass') goal'
        -- Check if goal is provable
        res <- io $ prove $ do
          vars <- genVars $ getVars newGoal
          toSmtB vars newGoal
        case show res of
          "Q.E.D." -> return "Pass"
          _ -> return "Fail"

-- | Assign a model (i.e. a result from the SAT solver) to a logical expression.
assign :: ResultMap -> Expr -> Expr
assign model = subst vs es
  where (vs, es) = unzip $ map (second $ LitInt . fromInteger) $ M.toList model

-- | Convert a GCL arithmetic expression to the corresponding SBV expression.
toSmt :: SBVars -> Expr -> Symbolic SInteger
toSmt _ (LitInt i) = return $ literal (toInteger i)
toSmt (vs, fvs, evs, _) (Name v) = return $
  fromMaybe (error "Inconsistent VarMap") (M.lookup v m)
  where m = foldr1 M.union [vs, fvs, evs]
toSmt vs (BinOp op e e') = do
  ve <- toSmt vs e
  ve' <- toSmt vs e'
  return $ smtOp op ve ve'
toSmt v@(_, _, _, uvs) (ArrayAccess a e) = do
  v <- toSmt v e
  return $ u v
  where
    u = fromMaybe (error $ "Inconsistent UVarMap: " ++ show a) (M.lookup a uvs)
toSmt vs (Cond g et ef) = do
  x <- free_
  vg <- toSmtB vs g
  vt <- toSmt vs et
  vf <- toSmt vs ef
  constrain $ vg ==> x .== vt
  constrain $ bnot vg ==> x .== vf
  return x
toSmt _ _ = error "toSmt cannot handle logical expressions"

smtOp Plus = (+)
smtOp Minus = (-)

-- | Convert a GCL logical expression to the corresponding SBV expression.
toSmtB :: SBVars -> Expr -> Symbolic SBool
toSmtB _ (LitBool b) = return $ fromBool b
toSmtB vs (BinOp op e e') =
  case op of
    Imply -> do
      ve <- toSmtB vs e
      ve' <- toSmtB vs e'
      return $ ve ==> ve'
    And -> do
      ve <- toSmtB vs e
      ve' <- toSmtB vs e'
      return $ ve &&& ve'
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

smtOpL Imply = (==>)
smtOpL Lt = (.<)

-- | Generate SBV variables.
genVars :: Vars -> Symbolic SBVars
genVars (vs, fvs, evs, uvs) = do
  vs' <- sIntegers vs
  fvs' <- mapM forall fvs
  evs' <- mapM exists evs
  let uvs' = map uninterpret uvs
  return ( M.fromList $ zip vs vs'
         , M.fromList $ zip fvs fvs'
         , M.fromList $ zip evs evs'
         , M.fromList $ zip uvs uvs'
         )

-- | Check whether an assumption reasons about arrays.
containsArray :: Expr -> Bool
containsArray ArrayAccess{} = True
containsArray Cond{} = True
containsArray (BinOp _ e e') = containsArray e || containsArray e'
containsArray (Not e) = containsArray e
containsArray (Forall _ e) = containsArray e
containsArray (Exist _ e) = containsArray e
containsArray _ = False

-- | Check whether an assumption contains free variables.
containsUVar :: [String] -> Expr -> Bool
containsUVar vs (Name x) = x `notElem` vs
containsUVar vs (BinOp _ e e') = containsUVar vs e || containsUVar vs e'
containsUVar vs (Not e) = containsUVar vs e
containsUVar _ Forall{} = True
containsUVar _ Exist{} = True
containsUVar _ _ = False

-- | Combine generated SBV variables
combineVars :: SBVars -> SBVars -> SBVars
combineVars (v, fv, ev, uv) (v', fv', ev', uv') =
  (M.union v v', M.union fv fv', M.union ev ev', M.union uv uv')

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

-- | Convert GCL's valid identifiers to SBV valid identifiers.
sanitize :: String -> String
sanitize ('$':s) = 'T' : s
sanitize s = s

sanitizeE :: Expr -> Expr
sanitizeE (Name x) = Name $ sanitize x
sanitizeE (Not e) = Not $ sanitizeE e
sanitizeE (BinOp op e1 e2) = BinOp op (sanitizeE e1) (sanitizeE e2)
sanitizeE (Cond g et ef) = Cond (sanitizeE g) (sanitizeE et) (sanitizeE ef)
sanitizeE (Forall v e) = Forall (map sanitize v) (sanitizeE e)
sanitizeE (Exist v e) = Exist (map sanitize v) (sanitizeE e)
sanitizeE (ArrayAccess v e) = ArrayAccess (sanitize v) (sanitizeE e)
sanitizeE e = e
