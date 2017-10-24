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

data Expr = LitInt Int
          | LitBool Bool
          | Name String
          | Plus Expr Expr
          | Minus Expr Expr
          | Imply Expr Expr
          | Not Expr
          | Lt Expr Expr
          | Eq Expr Expr
          | ArrayAccess String Expr
          | Forall [String] Expr
          | Cond Expr Expr Expr
          | RepBy Expr Expr Expr
          deriving Eq

-- data BoundVariable = BVar String Type

data PrimitiveType = Boolean | Integer deriving Show

data Type = Prim PrimitiveType | Array PrimitiveType

hoarify :: Program -> Expr -> Expr -> Program
hoarify prog pre post =
  prog { body = Seq (Assume pre) (Seq (body prog) (Assert post))}

-- Shorthands
and_ p q = Not $ Imply p (Not q)
or_ p q = Imply (Imply p q) q
le_ e1 e2 = or_ (Lt e1 e2) (Eq e1 e2)
gt e1 e2 = Not $ le_ e1 e2
ge e1 e2 = Not $ Lt e1 e2

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
  show (Plus e1 e2) = show e1 ++ " + " ++ show e2
  show (Minus e1 e2) = show e1 ++ " - " ++ show e2
  show (Imply p q) = "(" ++ show p ++ " => " ++ show q ++ ")"
  show (Not e) = "~ (" ++ show e ++ ")"
  show (Lt e1 e2) = show e1 ++ " < " ++ show e2
  show (Eq e1 e2) = show e1 ++ " = " ++ show e2
  show (ArrayAccess arr e) = arr ++ "[" ++ show e ++ "]"
  show (Forall vs e) =
    "(forall " ++ show vs ++ " :: " ++ show e ++ ")"
  show (Cond g et ef) =
    "(" ++ show g ++ " -> " ++ show et ++ " | " ++ show ef ++ ")"
  show (RepBy arr i e) =
    "[" ++ show arr ++ " | " ++ show i ++ " -> " ++ show e ++ "]"

instance Show Type where
  show (Prim primType) = show primType
  show (Array primType) = "[] " ++ show primType

-- instance Show BoundVariable where
--   show (BVar v t) = v ++ ":" ++ show t
