module AST where

data Program = Prog { name :: String
                    , inputs :: [String]
                    , outputs :: [String]
                    , body :: Stmt
                    }

data Stmt = Skip
          | Assume Expr
          | Assert Expr
          | Asg [String] [Expr]
          | Seq Stmt Stmt
          | Ite Expr Stmt Stmt
          | While (Maybe Expr) Expr Stmt
          | VarStmt [String] Stmt
          deriving ( Eq
                  --  , Show
                   )

data Expr = LitInt Int
          | LitBool Bool
          | Name String
          | Not Expr
          | BinOp Op Expr Expr
          | Cond Expr Expr Expr
          | Forall [String] Expr
          | ArrayAccess String Expr
          | RepBy Expr Expr Expr
          deriving ( Eq
                  --  , Show
                   )

data Op = Plus | Minus | Imply | Lt | Eq deriving ( Eq
                                                  -- , Show
                                                  )

hoarify :: Program -> Expr -> Expr -> Program
hoarify prog pre post =
  prog { body = Seq (Assume pre) (Seq (body prog) (Assert post)) }

-- DSL
infixr 4 <:>
s <:> s' = Seq s s'
infixr 5 .:=
vs .:= es = Asg vs es
infixr 3 ==>
e ==> e' = BinOp Imply e e'
infixl 2 \/
e \/ e' = (e ==> e') ==> e'
infixl 2 /\
e /\ e' = Not $ e ==> Not e'
infix 1 .=
e .= e' = BinOp Eq e e'
infix 1 .<
e .< e' = BinOp Lt e e'
infix 1 .<=
e .<= e' = (e .< e') \/ (e .= e')
infix 1 .>
e .> e' = Not $ e .<= e'
infix 1 .>=
e .>= e' = Not $ e .< e'
infixl 0 .+
e .+ e' = BinOp Plus e e'
infixl 0 .-
e .- e' = BinOp Minus e e'
_T = LitBool True
_F = LitBool False
i = LitInt
n = Name

-- Display
instance Show Program where
  show prog =
    "=============" ++ name prog ++ "================\n" ++
    show (body prog) ++
    "===============================================\n"
instance Show Stmt where
  show Skip = "Skip"
  show (Assert e) = "Assert (" ++ show e ++ ")"
  show (Assume e) = "Assume (" ++ show e ++ ")"
  show (Asg targets exprs) = show targets ++ " := " ++ show exprs
  show (Seq s1 s2) = show s1 ++ "; " ++ show s2
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
  show (BinOp op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (Not e) = "~ (" ++ show e ++ ")"
  show (ArrayAccess arr e) = arr ++ "[" ++ show e ++ "]"
  show (Forall vs e) =
    "(forall " ++ show vs ++ " :: " ++ show e ++ ")"
  show (Cond g et ef) =
    "(" ++ show g ++ " -> " ++ show et ++ " | " ++ show ef ++ ")"
  show (RepBy arr i e) =
    "[" ++ show arr ++ " | " ++ show i ++ " -> " ++ show e ++ "]"

instance Show Op where
  show op = case op of
    Plus -> "+"
    Minus -> "-"
    Lt -> "<"
    Eq -> "="
    Imply -> "==>"
