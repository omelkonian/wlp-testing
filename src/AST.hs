module AST where

data Program = Prog { name :: String
                    , inputs :: [String]
                    , outputs :: [String]
                    , body :: Stmt
                    }
                    -- deriving Show

data Stmt = Skip
          | Assert Expr
          | Assume Expr
          | Asg [String] [Expr]
          | Seq Stmt Stmt
          | Ite Expr Stmt Stmt
          | While Expr Stmt
          | VarStmt [String] Stmt
          -- deriving Show

data BoundVariable = BVar String Type
  deriving Show

data Expr = LitInt Int
          | LitBool Bool
          | Name String
          | Plus Expr Expr
          | Minus Expr Expr
          | Imply Expr Expr
          | Lt Expr Expr
          | Le Expr Expr
          | Eq Expr Expr
          | ArrayAccess String Expr
          | Not Expr
          | Forall BoundVariable Expr
          | Cond Expr Expr Expr
          | RepBy Expr Expr Expr
          deriving Show

data PrimitiveType = Boolean | Integer
  deriving Show

data Type = Prim PrimitiveType | Array PrimitiveType
  deriving Show

hoarify :: Program -> Expr -> Expr -> Program
hoarify prog pre post =
  prog { body = Seq (Assume pre) (Seq (body prog) (Assert post))}


instance Show Program where
  show prog =
    "=============" ++ name prog ++ "================\n" ++
    show (body prog) ++
    "===============================================\n"
instance Show Stmt where
  show Skip = "Skip"
  show (Assert e) = "Assert (" ++ show e ++ " )"
  show (Assume e) = "Assume (" ++ show e ++ ")"
  show (Asg targets exprs) = show targets ++ " := " ++ show exprs
  show (Seq s1 s2) = show s1 ++ "; " ++ show s2
  show (Ite g st sf) =
    "if (" ++ show g ++ ") then " ++ show st ++ " else " ++ show sf
  show (While g body) =
    "while (" ++ show g ++ ") do\n" ++ show body
  show (VarStmt vars body) =
    "var " ++ show vars ++ " in " ++ show body
