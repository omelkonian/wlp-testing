{-# LANGUAGE FlexibleContexts #-}

module Parser (programP) where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Tokens

import AST

-------------
-- Parsers --
-------------
test p = parse (p <* eof) ""
lexeme p = spaces *> p <* spaces
str = lexeme . string
(<~) p s = p <* lexeme (string s)
(~>) s p = lexeme (string s) *> p
parens = Tokens.parens haskell
identifier = Tokens.identifier haskell
reserved = Tokens.reservedOp haskell
infix_ op f = Infix (reserved op >> return f) AssocLeft
prefix op f = Prefix (reserved op >> return f)

-- Programs
programP :: Parser Program
programP = hoareProgramP <|> simpleProgramP

hoareProgramP :: Parser Program
hoareProgramP = do
  pre <- "{" ~> exprP <~ "}"
  prog <- simpleProgramP
  post <- "{" ~> exprP <~ "}"
  return $ hoarify prog pre post

simpleProgramP :: Parser Program
simpleProgramP = do
  name <- nameP <~ "("
  inputs <- (varP `sepBy` char ',') <~ "|"
  outputs <- (varP `sepBy` char ',') <~ ")"
  stmt <- stmtP
  return $ Prog name inputs outputs stmt

-- Statements
stmtP :: Parser Stmt
stmtP = buildExpressionParser table term <?> "expression"
  where table = [ [infix_ ";" Seq]]
        term = try varStmtP <|>
               try whileP <|>
               try iteP <|>
               try asgP <|>
               try assumeP <|>
               try assertP <|>
               skipP <?> "statement"

skipP :: Parser Stmt
skipP = Skip <$ str "skip"

assertP :: Parser Stmt
assertP = Assert <$> "assert" ~> exprP

assumeP :: Parser Stmt
assumeP = Assume <$> "assume" ~> exprP

asgP :: Parser Stmt
asgP = do
  lhs <- asgTargetP `sepBy1` char ','
  rhs <- ":=" ~> (exprP `sepBy1` char ',')
  return $ Asg lhs rhs

iteP :: Parser Stmt
iteP = do
  expr <- "if" ~> exprP
  stmtT <- "then" ~> stmtP
  stmtF <- "else" ~> stmtP
  return $ Ite expr stmtT stmtF

whileP :: Parser Stmt
whileP = do
  expr <- "while" ~> exprP
  stmt <- "do" ~> stmtP <~ "end"
  return $ While expr stmt

varStmtP :: Parser Stmt
varStmtP = do
  vars <- "var" ~> (varP `sepBy1` char ',')
  stmt <- "in" ~> stmtP <~ "end"
  return $ VarStmt vars stmt

asgTargetP :: Parser AsgTarget
asgTargetP = AsgName <$> nameP

-- Expressions
exprP :: Parser Expr
exprP = buildExpressionParser table term <?> "expression"
  where table = [ [infix_ "+" Plus, infix_ "-" Minus]
                , [infix_ "<" Lt, infix_ "<=" Le, infix_ "=" Eq]
                , [prefix "~" Not]
                , [infix_ "/\\" And]
                , [infix_ "\\/" Or]
                , [infix_ "~>" Imply]
                ]
        term = try (parens exprP) <|>
               try forallP <|>
               try arrayAccessP <|>
               primitiveP <?> "term"

varP :: Parser Variable
varP = Var <$> lexeme nameP

bvarP :: Parser BoundVariable
bvarP = lexeme $ do
  name <- nameP
  typ <- ":" ~> typeP
  return $ BVar name typ

forallP :: Parser Expr
forallP = "(" ~> body <~ ")"
  where body = do
         bvar <- "forall" ~> bvarP
         expr <- "::" ~> exprP
         return $ Forall bvar expr

arrayAccessP :: Parser Expr
arrayAccessP = do
  name <- nameP
  expr <- "[" ~> exprP <~ "]"
  return $ ArrayAccess name expr

primitiveP :: Parser Expr
primitiveP = lexeme $
  (LitInt <$> numberP)  <|>
  (LitBool <$> boolP) <|>
  (Name <$> nameP)

nameP :: Parser String
nameP = lexeme $ many1 letter

numberP :: Parser Int
numberP = (\n -> read n :: Int) <$> many1 digit

boolP :: Parser Bool
boolP = (True <$ str "true") <|> (False <$ str "false")

-- Types
typeP :: Parser Type
typeP = lexeme $
  (Prim <$> primitiveTypeP) <|>
  (Array <$> arrayTypeP)

primitiveTypeP :: Parser PrimitiveType
primitiveTypeP = lexeme $
  (Boolean <$ str "bool") <|>
  (Integer <$ str "int")

arrayTypeP :: Parser PrimitiveType
arrayTypeP = "[]" ~> primitiveTypeP
