module AST where

data Program = Prog { name :: String
                    , inputs :: [String]
                    , outputs :: [String]
                    , body :: Stmt
                    }
                    deriving Show

data Stmt = Skip
          | Assert Expr
          | Assume Expr
          | Asg [String] [Expr]
          | Seq Stmt Stmt
          | Ite Expr Stmt Stmt
          | While Expr Stmt
          | VarStmt [String] Stmt
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
