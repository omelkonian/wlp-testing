module AST where

-- | Representation of a GCL program.
data Program = Prog { name :: String
                    , inputs :: [String]
                    , outputs :: [String]
                    , body :: Stmt
                    , pre :: Expr
                    , post :: Expr
                    }

-- | Representation of a GCL statement.
data Stmt = Skip
          | Assume Expr
          | Assert Expr
          | Asg [String] [Expr]
          | Seq Stmt Stmt
          | Ite Expr Stmt Stmt
          | While (Maybe Expr) Expr Stmt
          | VarStmt [String] Stmt
          deriving Eq

-- | Representation of a GCL expression.
data Expr = LitInt Int
          | LitBool Bool
          | Name String
          | Not Expr
          | BinOp Op Expr Expr
          | Cond Expr Expr Expr
          | Forall [String] Expr
          | Exist [String] Expr
          | ArrayAccess String Expr
          | RepBy Expr Expr Expr
          | ProgCall String [Expr]
          deriving Eq

-- | Representation of a GCL operator.
data Op = Plus | Minus | Imply | And | Lt | Eq deriving Eq

-- | Markers used by Normalizer.
markAssumption = Forall ["$assumption"]
markGoal = Forall ["$goal"]

-- | Integrate (pre/post)-condition into the body of the program.
hoarify :: Program -> Program
hoarify prog =
  prog { body = Assume (pre prog)
            <:> body prog
            <:> Assert (markGoal $ post prog)
       }

-- Utility DSL.
infixr 1 <:>
s <:> s' = Seq s s'
infixr 2 .:=
vs .:= es = Asg vs es
infixr 3 ==>
e ==> e' = BinOp Imply e e'
infixl 5 \/
e \/ e' = (e ==> e') ==> e'
infixl 4 /\
e /\ e' = if e' == _T then e else BinOp And e e' -- Not $ e ==> Not e'
infix 6 .!=
e .!= e' = Not $ e .= e'
infix 6 .=
e .= e' = BinOp Eq e e'
infix 7 .<
e .< e' = BinOp Lt e e'
infix 7 .<=
e .<= e' = Not $ e' .< e
infix 7 .>
e .> e' = e' .< e
infix 7 .>=
e .>= e' = e' .<= e
infixl 8 .+
e .+ e' = BinOp Plus e e'
infixl 8 .-
e .- e' = BinOp Minus e e'
infixl 9 .!
a .! e = ArrayAccess a e
_T = LitBool True
_F = LitBool False
i = LitInt
n = Name

-- Display.
inParens s = "(" ++ s ++ ")"
instance Show Program where
  show prog =
    "=============" ++ name prog ++ "================\n" ++
    show (body prog) ++
    "===============================================\n"
instance Show Stmt where
  show Skip = "Skip"
  show (Assert e) = "Assert (" ++ show e ++ ")"
  show (Assume e) = "Assume (" ++ show e ++ ")"
  show (Asg targets exprs) = inParens $ show targets ++ " := " ++ show exprs
  show (Seq s1 s2) = inParens $ show s1 ++ "; " ++ show s2
  show (Ite g st sf) =
    "if (" ++ show g ++ ") then " ++ show st ++ " else " ++ show sf
  show (While _ g body) =
    "while (" ++ show g ++ ") do\n" ++ show body ++ "end"
  show (VarStmt vars body) =
    "var " ++ show vars ++ " in " ++ show body ++ " end"

instance Show Expr where
  show (LitInt i) = show i
  show (LitBool b) = show b
  show (Name s) = s
  show (BinOp op e1 e2) = inParens $ show e1 ++ " " ++ show op ++ " " ++ show e2
  show (Not e) = "~ (" ++ show e ++ ")"
  show (ArrayAccess arr e) = arr ++ "[" ++ show e ++ "]"
  show (Forall vs e) =
    "(forall " ++ show vs ++ " :: " ++ show e ++ ")"
  show (Exist vs e) =
    "(exist " ++ show vs ++ " :: " ++ show e ++ ")"
  show (Cond g et ef) =
    "(" ++ show g ++ " -> " ++ show et ++ " | " ++ show ef ++ ")"
  show (RepBy arr i e) =
    "[" ++ show arr ++ " | " ++ show i ++ " -> " ++ show e ++ "]"
  show (ProgCall p es) =
    p ++ "(" ++ show es ++ ")"

instance Show Op where
  show op = case op of
    Plus -> "+"
    Minus -> "-"
    Lt -> "<"
    Eq -> "="
    Imply -> "==>"
    And -> "/\\"
