module AST where

data Program = Prog { name :: String
                    , inputs :: [Variable]
                    , outputs :: [Variable]
                    , body :: Stmt
                    }
                    deriving Show

data Stmt = Skip
          | Assert Expr
          | Assume Expr
          | Asg [AsgTarget] [Expr]
          | Seq Stmt Stmt
          | Ite Expr Stmt Stmt
          | While Expr Stmt
          | VarStmt [Variable] Stmt
          deriving Show

newtype AsgTarget = AsgName String
  deriving Show

newtype Variable = Var String
  deriving Show

data BoundVariable = BVar String Type
  deriving Show

data Expr = LitInt Int
          | LitBool Bool
          | Name String
          | Plus Expr Expr
          | Minus Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Imply Expr Expr
          | Lt Expr Expr
          | Le Expr Expr
          | Eq Expr Expr
          | ArrayAccess String Expr
          | Not Expr
          | Forall BoundVariable Expr
          deriving Show

data PrimitiveType = Boolean | Integer
  deriving Show

data Type = Prim PrimitiveType | Array PrimitiveType
  deriving Show

hoarify :: Program -> Expr -> Expr -> Program
hoarify prog pre post =
  prog { body = Seq (Assume pre) (Seq (body prog) (Assert post))}
